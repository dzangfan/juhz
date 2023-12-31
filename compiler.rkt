#lang racket

(require threading)
(require basic-cc/visitor)
(require basic-cc/tokenization)
(require "interpreter.rkt")
(require "language.rkt")

(begin-for-syntax
  (define temporary-variable-id-pool 0))

(define-syntax (with-constructing-temporary-variable stx)
  (syntax-case stx ()
    [(_ [name* value-ast*] expr)
     (begin
       (set! temporary-variable-id-pool (add1 temporary-variable-id-pool))
       (define variable-name (format "temporary_~A" temporary-variable-id-pool))
       (begin0
           (with-syntax ([variable-name (datum->syntax #'stx variable-name #'stx)])
             #'(let* ([value value-ast*]
                      [parse-tree (get-field parse-tree value)]
                      [name* (new identifier% (name variable-name) (parse-tree parse-tree))]
                      [result-ast expr])
                 (new program%
                      (parse-tree parse-tree)
                      (statement-ast-list
                       (list (new definition% (name variable-name) (value-ast value))
                             result-ast)))))
         (set! temporary-variable-id-pool (sub1 temporary-variable-id-pool))))]))

(define (construct-hook-invocation hook-name receiver-ast argument-ast-list hook-parse-tree whole-parse-tree)
  (with-constructing-temporary-variable (receiver receiver-ast)
    (let ([selection (new selection%
                          (target-ast receiver)
                          (property-name hook-name)
                          (parse-tree hook-parse-tree))])
      (new call%
           (caller-ast selection)
           (argument-ast-list (cons receiver argument-ast-list))
           (suffix-ast #f) (suffix-type #f)
           (parse-tree whole-parse-tree)))))

(define (token/IDENT->token/STRING ident)
  (token 'STRING
         (format "\"~A\"" (token-text ident))
         (token-location ident)))

(define operation-tag-list0~5
  '(operation operation#0 operation#1 operation#2
              operation#3 operation#4 operation#5))

(define operation-tag-list0~6
  (cons 'operation#6 operation-tag-list0~5))

(define-visitor flatten
  [(parameter-list @IDENT) (list IDENT)]
  [(parameter-list @IDENT COMMA @parameter-list)
   (cons IDENT (flatten parameter-list))]
  [(argument-list @expression) (list expression)]
  [(argument-list @expression COMMA @argument-list)
   (cons expression (flatten argument-list))]
  [($operation $any)
   #:when (memq operation operation-tag-list0~5)
   (flatten any)]
  [($operation $left-operand $operator $right-operand)
   #:when (memq operation operation-tag-list0~5)
   (list left-operand (second operator) right-operand)]
  [(operation#6 $operator $operand)
   (list (second operator) operand)]
  [(operation#6 $any)
   (list any)])

(define-visitor break-call-up
  [(call @callable ROUNDLEFT ROUNDRIGHT)
   (values callable null)]
  [(call @callable ROUNDLEFT @argument-list ROUNDRIGHT)
   (values callable (flatten argument-list))])

(define-visitor compile/parse-tree
  [(program @statement)
   (new program%
        (statement-ast-list (list (compile/parse-tree statement)))
        (parse-tree *parse-tree*))]
  [(program @statement @program)
   (define program-ast (compile/parse-tree program))
   (new program%
        (statement-ast-list (cons (compile/parse-tree statement) (get-field statement-ast-list program-ast)))
        (parse-tree *parse-tree*))]
  [(statement @right-value) (compile/parse-tree right-value)]
  [(statement USE @right-value)
   (new use%
        (expression-ast (compile/parse-tree right-value))
        (parse-tree *parse-tree*))]
  [(statement DEF (left-value @IDENT) EQ @right-value)
   (new definition%
        (name (token-text IDENT))
        (value-ast (compile/parse-tree right-value))
        (parse-tree *parse-tree*))]
  [(statement DEF (left-value @IDENT ROUNDLEFT ROUNDRIGHT) EQ @right-value)
   (new function-definition%
        (name (token-text IDENT))
        (argument-name-list null)
        (body-ast/basic-program
         (compile/parse-tree right-value))
        (parse-tree *parse-tree*))]
  [(statement DEF (left-value @IDENT ROUNDLEFT @parameter-list ROUNDRIGHT) EQ @right-value)
   (new function-definition%
        (name (token-text IDENT))
        (argument-name-list (~>> parameter-list flatten (map token-text)))
        (body-ast/basic-program (compile/parse-tree right-value))
        (parse-tree *parse-tree*))]
  [(statement DEF (left-value PACKAGE DOT @IDENT) EQ @right-value)
   (new package-definition%
        (name (token-text IDENT))
        (value-ast (compile/parse-tree right-value))
        (parse-tree *parse-tree*))]
  [(statement DEF (left-value @IDENT DOT $name) @EQ @right-value)
   (define definition-invocation
     (construct-hook-invocation "__DEFINE__"
                                (new identifier% (name (token-text IDENT)) (parse-tree IDENT))
                                (list (new constant%
                                           (constant-token (token/IDENT->token/STRING name))
                                           (parse-tree name))
                                      (compile/parse-tree right-value))
                                EQ *parse-tree*))
   (new use%
        (expression-ast definition-invocation)
        (parse-tree *parse-tree*))]
  [(statement (callable @IDENT) EQ @right-value)
   (new assignment%
        (name (token-text IDENT))
        (value-ast (compile/parse-tree right-value))
        (parse-tree *parse-tree*))]
  [(statement (callable (selection @callable DOT @IDENT)) EQ @right-value)
   (new selection-assignment%
        (prefix-ast (compile/parse-tree callable))
        (name (token-text IDENT))
        (value-ast (compile/parse-tree right-value))
        (parse-tree *parse-tree*))]
  [(statement (callable (selection PACKAGE DOT @IDENT)) EQ @right-value)
   (new package-assignment%
        (name (token-text IDENT))
        (value-ast (compile/parse-tree right-value))
        (parse-tree *parse-tree*))]
  [(statement (callable @call) EQ @right-value)
   (define-values (callable-parse-tree argument-parse-tree-list) (break-call-up call))
   (new call%
        (caller-ast (compile/parse-tree callable-parse-tree))
        (argument-ast-list (map compile/parse-tree
                                (append argument-parse-tree-list (list right-value))))
        (suffix-ast #f) (suffix-type #f)
        (parse-tree *parse-tree*))]
  [(statement (callable (indexing @callable SQUARELEFT @expression SQUARERIGHT)) @EQ @right-value)
   (construct-hook-invocation "__INDEX__" (compile/parse-tree callable)
                              (list (compile/parse-tree expression) (compile/parse-tree right-value))
                              EQ *parse-tree*)]
  [(right-value @call COLON (right-value @function))
   (define-values (caller argument-parse-tree-list) (break-call-up call))
   (new call%
        (caller-ast (compile/parse-tree caller))
        (argument-ast-list (map compile/parse-tree argument-parse-tree-list))
        (suffix-ast (compile/parse-tree function))
        (suffix-type 'function)
        (parse-tree *parse-tree*))]
  [(right-value @call COLON (right-value @package))
   (define-values (caller argument-parse-tree-list) (break-call-up call))
   (new call%
        (caller-ast (compile/parse-tree caller))
        (argument-ast-list (map compile/parse-tree argument-parse-tree-list))
        (suffix-ast (compile/parse-tree package))
        (suffix-type 'package)
        (parse-tree *parse-tree*))]
  [(right-value @call COLON @right-value)
   (define-values (caller argument-parse-tree-list) (break-call-up call))
   (new call%
        (caller-ast (compile/parse-tree caller))
        (argument-ast-list (map compile/parse-tree argument-parse-tree-list))
        (suffix-ast (compile/parse-tree right-value))
        (suffix-type 'normal)
        (parse-tree *parse-tree*))]
  [(right-value @expression SEMICOLON) (compile/parse-tree expression)]
  [(right-value CURLYLEFT @program CURLYRIGHT) (compile/parse-tree program)]
  [(right-value $any) (compile/parse-tree any)]
  [(expression $any) (compile/parse-tree any)]
  [(atom ROUNDLEFT @expression ROUNDRIGHT) (compile/parse-tree expression)]
  [(atom $any) (compile/parse-tree any)]
  [@NUMBER (new constant% (constant-token NUMBER) (parse-tree *parse-tree*))]
  [@STRING (new constant% (constant-token STRING) (parse-tree *parse-tree*))]
  [@TRUE (new constant% (constant-token TRUE) (parse-tree *parse-tree*))]
  [@FALSE (new constant% (constant-token FALSE) (parse-tree *parse-tree*))]
  [@IDENT (new identifier% (name (token-text IDENT)) (parse-tree *parse-tree*))]
  [(callable $any) (compile/parse-tree any)]
  [@call (define-values (caller argument-parse-tree-list) (break-call-up call))
         (new call%
              (caller-ast (compile/parse-tree caller))
              (argument-ast-list (map compile/parse-tree argument-parse-tree-list))
              (suffix-ast #f) (suffix-type #f)
              (parse-tree *parse-tree*))]
  [(array SQUARELEFT SQUARERIGHT)
   (new array% (ast-list null) (parse-tree *parse-tree*))]
  [(array SQUARELEFT @argument-list SQUARERIGHT)
   (new array%
        (ast-list (~>> argument-list flatten (map compile/parse-tree)))
        (parse-tree *parse-tree*))]
  [(package PACKAGE CURLYLEFT @program CURLYRIGHT)
   (define program-ast (compile/parse-tree program))
   (new package%
        (public-name-list #f)
        (body-ast/collector-program
         (new collector-program%
              (statement-ast-list (get-field statement-ast-list program-ast))
              (parse-tree program)))
        (parse-tree *parse-tree*))]
  [(package PACKAGE ROUNDLEFT ROUNDRIGHT CURLYLEFT @program CURLYRIGHT)
   (define program-ast (compile/parse-tree program))
   (new package%
        (public-name-list null)
        (body-ast/collector-program
         (new collector-program%
              (statement-ast-list (get-field statement-ast-list program-ast))
              (parse-tree program)))
        (parse-tree *parse-tree*))]
  [(package PACKAGE ROUNDLEFT @parameter-list ROUNDRIGHT CURLYLEFT @program CURLYRIGHT)
   (define program-ast (compile/parse-tree program))
   (new package%
        (public-name-list (~>> parameter-list flatten (map token-text)))
        (body-ast/collector-program
         (new collector-program%
              (statement-ast-list (get-field statement-ast-list program-ast))
              (parse-tree program)))
        (parse-tree *parse-tree*))]
  [(function FUNCTION CURLYLEFT @program CURLYRIGHT)
   (new function%
        (argument-name-list null)
        (body-ast/basic-program (compile/parse-tree program))
        (parse-tree *parse-tree*))]
  [(function FUNCTION ROUNDLEFT ROUNDRIGHT CURLYLEFT @program CURLYRIGHT)
   (new function%
        (argument-name-list null)
        (body-ast/basic-program (compile/parse-tree program))
        (parse-tree *parse-tree*))]
  [(function FUNCTION ROUNDLEFT @parameter-list ROUNDRIGHT CURLYLEFT @program CURLYRIGHT)
   (new function%
        (argument-name-list (~>> parameter-list flatten (map token-text)))
        (body-ast/basic-program (compile/parse-tree program))
        (parse-tree *parse-tree*))]
  [(condition IF $condition CURLYLEFT $true-case CURLYRIGHT ELSE CURLYLEFT $false-case CURLYRIGHT)
   (new condition%
        (cond-ast (compile/parse-tree condition))
        (true-case-ast (compile/parse-tree true-case))
        (false-case-ast (compile/parse-tree false-case))
        (parse-tree *parse-tree*))]
  [(loop WHILE $condition CURLYLEFT @program CURLYRIGHT)
   (new loop% (cond-ast (compile/parse-tree condition)) (body-ast (compile/parse-tree program)) (parse-tree *parse-tree*))]
  [(selection @callable DOT @IDENT)
   (new selection%
        (target-ast (compile/parse-tree callable))
        (property-name (token-text IDENT))
        (parse-tree *parse-tree*))]
  [(selection PACKAGE DOT @IDENT)
   (new package-selection% (name (token-text IDENT)) (parse-tree *parse-tree*))]
  [(indexing @callable @SQUARELEFT @expression SQUARERIGHT)
   (define callable-ast (compile/parse-tree callable))
   (construct-hook-invocation "__INDEX__" callable-ast (list (compile/parse-tree expression))
                              SQUARELEFT *parse-tree*)]
  [$operation
   #:when (and (pair? operation) (memq (first operation) operation-tag-list0~6))
   (match (flatten operation)
     [(list any) (compile/parse-tree any)]
     [(list operator operand)
      (define operand-ast (compile/parse-tree operand))
      (construct-hook-invocation (format "__~A__" (token-type operator))
                                 operand-ast (list) operator *parse-tree*)]
     [(list left-operand (struct token ('AND _ _)) right-operand)
      (new condition%
           (cond-ast (compile/parse-tree left-operand))
           (true-case-ast (compile/parse-tree right-operand))
           (false-case-ast (make-object/BOOLEAN #f))
           (parse-tree *parse-tree*))]
     [(list left-operand (struct token ('OR _ _)) right-operand)
      (new condition%
           (cond-ast (compile/parse-tree left-operand))
           (true-case-ast 'same-as-condition)
           (false-case-ast (compile/parse-tree right-operand))
           (parse-tree *parse-tree*))]
     [(list left-operand operator right-operand)
                 (define left-operand-ast (compile/parse-tree left-operand))
                 (construct-hook-invocation (format "__~A__" (token-type operator))
                                            left-operand-ast
                                            (list (compile/parse-tree right-operand))
                                            operator *parse-tree*)])]
  [$others (report/internal-error *parse-tree* "Unknown parse tree ~A" *parse-tree*)])

(define (juhz-compile in #:file [file "(string)"])
  (~> (juhz-read in #:file file) compile/parse-tree))

(provide juhz-compile)
