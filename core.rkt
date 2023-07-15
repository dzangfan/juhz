#lang racket

(require basic-cc/language)
(require basic-cc/visitor)
(require basic-cc/tokenization)
(require threading)

(define-language juhz
  "\\s+"
  (EQ "=") (SEMICOLON ";") (DOT "\\.") (COMMA ",") (COLON ":")
  (PLUS "\\+") (MINUS "-") (TIMES "\\*") (DIVIDE "/") (REM "%")
  (AND "&&") (OR "\\|\\|") (BANG "!")
  (LT "<") (GT ">") (SAME "==") (DIFF "!=") (LE "<=") (GE ">=")
  (ROUNDLEFT "\\(") (ROUNDRIGHT "\\)")
  (SQUARELEFT "\\[") (SQUARERIGHT "\\]")
  (CURLYLEFT "\\{") (CURLYRIGHT "\\}")
  (IDENT "[a-zA-Z_][a-zA-Z0-9_]*")
  (NUMBER "(?:[1-9][0-9]*)|0|(?:(?:[1-9][0-9]*)?\\.[0-9]+)")
  (STRING "\"(?:\\\\\"|\\\\n|\\\\\\\\|[^\"\\\\])*\"")
  (TRUE "true") (FALSE "false")
  (IF "if") (ELSE "else") (WHILE "while") (PACKAGE "package")
  (FUNCTION "function") (DEF "def") (USE "use")
  (program statement (statement program))
  ;; use package.stdio;
  ;; println("Hello world!\n");
  ;; def name = "Juhz";
  ;; def name = {
  ;;   println("...");
  ;;   "Juhz";
  ;; }
  ;; def isEven(n) = n % 2 == 0;
  ;; def isOdd(n) = {
  ;;   !isEven(n)
  ;; }
  ;; def Employee(name, position) = package {
  ;;   name = name;
  ;;   email = format("~A.~A@mail.com", name, position);
  ;; }
  ;; if isOdd(2) {
  ;;   println("yes");
  ;; } else {
  ;;   println("no");
  ;; }
  ;; while x < 10 {
  ;;   x = x + 1
  ;; }
  ;; when(x < 10): println(x);
  ;; when(x < 10): {
  ;;   println(x);
  ;; }
  ;; when(getHash(tbl, "foo")) (it) {
  ;;   println(it);
  ;; }
  (statement (expression SEMICOLON)
             (USE expression SEMICOLON)
             (DEF left-value EQ right-value)
             (IDENT EQ right-value)
             (IF expression CURLYLEFT program CURLYRIGHT)
             (IF expression CURLYLEFT program CURLYRIGHT ELSE CURLYLEFT program CURLYRIGHT)
             (WHILE expression CURLYLEFT program CURLYRIGHT)
             (call COLON expression SEMICOLON)
             (call COLON CURLYLEFT program CURLYRIGHT)
             (call COLON ROUNDLEFT parameter-list ROUNDRIGHT CURLYLEFT program CURLYRIGHT))
  (left-value IDENT (IDENT ROUNDLEFT ROUNDRIGHT) (IDENT ROUNDLEFT parameter-list ROUNDRIGHT))
  (right-value (expression SEMICOLON)
               (CURLYLEFT program CURLYRIGHT)
               package
               function)
  (parameter-list IDENT (IDENT COMMA parameter-list))
  (expression array package function operation)
  (atom NUMBER STRING TRUE FALSE)
  (callable IDENT call indexing selection)
  (call (callable ROUNDLEFT ROUNDRIGHT) (callable ROUNDLEFT argument-list ROUNDRIGHT))
  ;; [1, 1, a, b, a + b]
  (array (SQUARELEFT SQUARERIGHT) (SQUARELEFT argument-list SQUARERIGHT))
  ;; package { def foo = bar; }
  ;; package() { def foo = bar; }
  ;; package(foo) { def bar = 10; def foo = bar; }
  (package (PACKAGE CURLYLEFT program CURLYRIGHT)
           (PACKAGE ROUNDLEFT ROUNDRIGHT CURLYLEFT program CURLYRIGHT)
           (PACKAGE ROUNDLEFT parameter-list ROUNDRIGHT CURLYLEFT program CURLYRIGHT))
  ;; function { println("bang!"); }
  ;; function() { println("bang!"); }
  ;; function(x) { x + 1; }
  (function (FUNCTION CURLYLEFT program CURLYRIGHT)
            (FUNCTION ROUNDLEFT ROUNDRIGHT CURLYLEFT program CURLYRIGHT)
            (FUNCTION ROUNDLEFT parameter-list ROUNDRIGHT CURLYLEFT program CURLYRIGHT))
  ;; array[10]
  ;; assoc(elt, lst)[2]
  ;; matrix[0][1]
  ;; company.staff[0]
  (indexing (callable SQUARELEFT expression SQUARERIGHT))
  ;; object.name
  ;; pwd().name
  ;; objects[0].name
  ;; company.staff.name
  ;; company.staff.retire("now")
  (selection (callable DOT IDENT)
             (PACKAGE DOT IDENT))
  (argument-list expression (expression COMMA argument-list))
  (operation operation#0)
  (operation#0 operation#1 (expression operator#0 operation#1))
  (operator#0 OR)
  (operation#1 operation#2 (operation#1 operator#1 operation#2))
  (operator#1 AND)
  (operation#2 operation#3 (operation#2 operator#2 operation#3))
  (operator#2 SAME DIFF)
  (operation#3 operation#4 (operation#3 operator#3 operation#4))
  (operator#3 LT GT LE GE)
  (operation#4 operation#5 (operation#4 operator#4 operation#5))
  (operator#4 PLUS MINUS)
  (operation#5 operation#6 (operation#5 operator#5 operation#6))
  (operator#5 TIMES DIVIDE REM)
  (operation#6 atom callable (operator#6 operation#6))
  (operator#6 BANG PLUS MINUS))

(define operation-list
  '(operation#0 operation#1 operation#2 operation#3 operation#4 operation#5 operation#6))

(define (repl tag lst)
  (cons tag (rest lst)))

(define (cons2 elt lst)
  (list* (first lst)
         elt
         (rest lst)))

(define (cons-to-2 elt lst)
  (match lst
    [(list first second rest ...)
     (list* first (cons elt second) rest)]))

(define (append-to-2 elt lst)
  (match lst
    [(list first second rest ...)
     (list* first (append second (list elt)) rest)]))

(define (insert-code-block simplified-call simplified-expression/program [simplified-parameter-list null])
  (define add-parameter-list
    (if (null? simplified-parameter-list)
        identity
        (lambda (function)
          (cons2 simplified-parameter-list function))))
  (define function
    (match simplified-expression/program
      [(and program (list 'program _ ...))
       (~>> program (repl 'function) add-parameter-list)]
      [expression
       (~>> expression list (cons 'function) add-parameter-list)]))
  (match simplified-call
    [(list 'call callable)
     `(call ,callable (argument-list (,function)))]
    [(list 'call callable (list 'argument-list (list arguments ...)))
     `(call ,callable (argument-list (,@arguments ,function)))]))

(define-visitor simplify
  [($operation $expression)
   #:when (memq operation operation-list)
   (simplify expression)]
  [(operation $expression) (simplify expression)]
  [($operation $operand#0 (_ $operator) $operand#1)
   #:when (memq operation operation-list)
   (list 'operation (simplify operand#0) operator (simplify operand#1))]
  [(operation#6 (_ $operator) $operand)
   (list 'operation operator (simplify operand))]
  [(program @statement)
   (list 'program (list (simplify statement)))]
  [(program @statement @program)
   (cons-to-2 (simplify statement) (simplify program))]
  [(statement USE @expression SEMICOLON)
   (list 'use (simplify expression))]
  [(statement @expression SEMICOLON) (simplify expression)]
  [(statement DEF @left-value EQ @right-value)
   (list 'definition (simplify left-value) (simplify right-value))]
  [(statement @IDENT EQ @right-value)
   (list 'assignment IDENT (simplify right-value))]
  [(statement @call COLON @expression _)
   (insert-code-block (simplify call) (simplify expression))]
  [(statement @call COLON CURLYLEFT @program CURLYRIGHT)
   (insert-code-block (simplify call) (simplify program))]
  [(statement @call COLON ROUNDLEFT @parameter-list ROUNDRIGHT CURLYLEFT @program CURLYRIGHT)
   (insert-code-block (simplify call) (simplify program) (simplify parameter-list))]
  [(left-value @IDENT) IDENT]
  [(left-value @IDENT _ _) (list 'function IDENT)]
  [(left-value @IDENT _ @parameter-list _)
   (list 'function IDENT (simplify parameter-list))]
  [(right-value @expression SEMICOLON) (simplify expression)]
  [(right-value CURLYLEFT @program CURLYRIGHT) (simplify program)]
  [(right-value @package) (simplify package)]
  [(right-value @function) (simplify function)]
  [(parameter-list @IDENT) (list 'parameter-list (list IDENT))]
  [(parameter-list @IDENT COMMA @parameter-list)
   (cons-to-2 IDENT (simplify parameter-list))]
  [(statement IF $condition _ @program _)
   (list 'if (simplify condition) (simplify program))]
  [(statement IF $condition _ $true-case _ _ _ $false-case _)
   (list 'if (simplify condition) (simplify true-case) (simplify false-case))]
  [(statement WHILE $condition _ @program _)
   (list 'while (simplify condition) (simplify program))]
  [(expression $any) (simplify any)]
  [(atom $any) any]
  [(callable $any) (simplify any)]
  [(call @callable _ _) (list 'call (simplify callable))]
  [(call @callable _ @argument-list _)
   (list 'call (simplify callable) (simplify argument-list))]
  [(array _ _) (list 'array)]
  [(array _ @argument-list _) (repl 'array (simplify argument-list))]
  [(package _ _ @program _) (repl 'package (simplify program))]
  [(package _ _ _ _ @program _)
   (cons2 (list 'parameter-list) (repl 'package (simplify program)))]
  [(package _ _ @parameter-list _ _ @program _)
   (cons2 (simplify parameter-list) (repl 'package (simplify program)))]
  [(function _ _ @program _) (repl 'function (simplify program))]
  [(function _ _ _ _ @program _)
   (cons2 (list 'parameter-list) (repl 'function (simplify program)))]
  [(function _ _ @parameter-list _ _ @program _)
   (cons2 (simplify parameter-list) (repl 'function (simplify program)))]
  [(indexing @callable _ @expression _) (list 'indexing (simplify callable) (simplify expression))]
  [(selection (callable @selection) DOT @IDENT)
   (append-to-2 IDENT (simplify selection))]
  [(selection PACKAGE DOT @IDENT)
   (list 'selection/package (list IDENT))]
  [(selection @callable DOT @IDENT)
   (list 'selection (list (simplify callable) IDENT))]
  [(argument-list @expression)
   (list 'argument-list (list (simplify expression)))]
  [(argument-list @expression COMMA @argument-list)
   (cons-to-2 (simplify expression) (simplify argument-list))]
  [($wrapper $any)
   (list wrapper (simplify any))]
  [$others others])

(define (juhz-read/AST in #:file [file "(string)"])
  (simplify (juhz-read in #:file file)))

(provide juhz-cut juhz-read juhz-read/AST (rename-out [simplify juhz-simplify]))

(struct juhz-package (property using) #:transparent)

;; NUMBER
;; STRING
;; BOOLEAN
;; ARRAY
;; PACKAGE
;; FUNCTION
(struct juhz-object (type value package) #:transparent)

(define (empty-package) (juhz-package #hash() null))

(define root-package (empty-package))

(define (root-package? package) (eq? package root-package))

(define (lookup-in-package/direct package name)
  (~> package juhz-package-property (hash-ref name #f)))

(define (lookup-in-package package name)
  (and (not (root-package? package))
       (lookup-in-package/direct package name)
       (for/first ([using-package (juhz-package-using package)]
                   #:do [(define result (lookup-in-package using-package))]
                   #:when result)
         result)))

(define (extend-package package name object)
  (match package [(struct juhz-package (property using))
                  (juhz-package (hash-set property name object)
                                using)]))

(define juhz-TRUE (juhz-object 'BOOLEAN true (empty-package)))

(define juhz-FALSE (juhz-object 'BOOLEAN false (empty-package)))

(define (run-hook name name-location object arguments)
  (define hook (format "__~A__" name))
  (define hook/IDENT (token 'IDENT hook name-location))
  `(call (selection ,object ,hook/IDENT) (argument-list ,@arguments)))

#;(define-visitor (eval-AST environment)
    [(operation $operand#0 OR $operand#1)
     (define first-result (eval-AST operand#0))
     (if (eq? juhz-FALSE first-result)
         (eval-AST operand#1)
         first-result)]
    [(operation $operand#0 AND $operand#1)
     (define first-result (eval-AST operand#0))
     (if (eq? juhz-FALSE first-result)
         juhz-FALSE
         (eval-AST operand#1))]
    [(operation $operand#0 $operator $operand#1)
     (~> (run-hook (token-type operator) (token-location operator) operand#0 operand#1)
         eval-AST)]
    [(program)
     0])
