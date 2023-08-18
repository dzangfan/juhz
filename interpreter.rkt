#lang racket

(require basic-cc/tokenization)
(require threading)

(struct package (direct-mapping linked-package-list) #:transparent)

(define no-more-package null)

(define root-package (package (make-hash) no-more-package))

(define (extend-package parent-package)
  (package (make-hash) (list parent-package)))

(define (using-package current-package used-package)
  (match-define (struct package (direct-mapping linked-package-list)) current-package)
  (package direct-mapping (cons used-package linked-package-list)))

(define (find-in-package current-package name)
  (cond [(eq? current-package no-more-package) #f]
        [else (match-define (struct package (direct-mapping linked-package-list)) current-package)
              (if (hash-has-key? direct-mapping name)
                  (hash-ref direct-mapping name)
                  (for/first ([used-package (in-list linked-package-list)]
                              #:do [(define result (find-in-package used-package name))]
                              #:when result)
                    result))]))

(define (modify-in-package! current-package name value)
  (cond [(eq? current-package root-package) #f]
        [else (match-define (struct package (direct-mapping linked-package-list)) current-package)
              (if (hash-has-key? direct-mapping name)
                  (begin (hash-set! direct-mapping name value) #t)
                  (let try-in-linked-packages ([rest-packages linked-package-list])
                    (match rest-packages
                      [(list) #f]
                      [(list used-package rest-packages+ ...)
                       (or (modify-in-package! used-package name value)
                           (try-in-linked-packages rest-packages+))])))]))

(define (define-in-package current-package name value)
  (match-define (struct package (direct-mapping linked-package-list)) current-package)
  (define direct-mapping* (hash-copy direct-mapping))
  (hash-set! direct-mapping* name value)
  (package direct-mapping* linked-package-list))

(define (define-in-package! current-package name value)
  (match-define (struct package (direct-mapping linked-package-list)) current-package)
  (hash-set! direct-mapping name value)
  current-package)

(define (modify-root-package! name value)
  (hash-set! (package-direct-mapping root-package)
             name value))

(struct object/function (argument-name-list body-ast package/env) #:transparent)

(struct object (type value package) #:transparent #:mutable)

(struct result (object package/env reserved-data) #:transparent)

(define (find-field object name)
  (find-in-package (object-package object) name))

(define base-package/NUMBER
  (box (extend-package root-package)))

(define base-package/STRING
  (box (extend-package root-package)))

(define base-package/BOOLEAN
  (box (extend-package root-package)))

(define base-package/ARRAY
  (box (extend-package root-package)))

(define base-package/FUNCTION
  (box (extend-package root-package)))

(define (make-object/NUMBER number)
  (object 'number number (extend-package (unbox base-package/NUMBER))))

(define (encode-string string)
  (let build ([rest-chars (string->list string)] [collected-chars null])
    (match rest-chars
      [(list) (~> collected-chars reverse list->string)]
      [(list #\\ #\n rest-chars ...) (build rest-chars (cons #\newline collected-chars))]
      [(list #\\ char rest-chars ...) (build rest-chars (cons char collected-chars))]
      [(list char rest-chars ...) (build rest-chars (cons char collected-chars))])))

(define (make-object/STRING+escape string)
  (let ([content (substring string 1 (sub1 (string-length string)))])
    (make-object/STRING (encode-string content))))

(define (make-object/STRING content)
  (object 'string content (extend-package (unbox base-package/STRING))))

(define (make-object/BOOLEAN true?)
  (object 'boolean true? (extend-package (unbox base-package/BOOLEAN))))

(define (object-true? any-object)
  (match any-object
    [(struct object ('boolean #false _)) #f]
    [else #t]))

(define (make-object/PACKAGE package)
  (object 'package #f package))

(define (make-object/FUNCTION argument-name-list body-ast package/env)
  (object 'function (object/function argument-name-list body-ast package/env)
          (extend-package (unbox base-package/FUNCTION))))

(define (make-object/ARRAY vector)
  (object 'array vector (extend-package (unbox base-package/ARRAY))))

(define object/NOT-PROVIDED
  (object 'not-provided #f (extend-package root-package)))

(define object/UNDEFINED
  (object 'undefined #f (extend-package root-package)))

(define library-package-table (make-hash))

(define (library-package-set! package-name package-object)
  (hash-set! library-package-table package-name package-object))

(define (library-package-ref package-name)
  (hash-ref library-package-table package-name #f))

(define (parse-tree->location-string parse-tree)
  (define (parse-tree->oneside-location side-function parse-tree)
    (if (token? parse-tree)
        (token-location parse-tree)
        (let ([children (rest parse-tree)])
          (parse-tree->oneside-location side-function (side-function children)))))
  (define left-most (parse-tree->oneside-location first parse-tree))
  (define right-most (parse-tree->oneside-location last parse-tree))
  (if (equal? left-most right-most)
      (format "~A: L~AC~A" (location-file left-most)
              (add1 (location-line left-most)) (add1 (location-column left-most)))
      (format "~A: L~AC~A...L~AC~A" (location-file left-most)
              (add1 (location-line left-most)) (add1 (location-column left-most))
              (add1 (location-line right-most)) (add1 (location-column right-most)))))

(define ((report constructor) causal-parse-tree format-string . format-args)
  (raise (constructor (format "~A~%See [~A]"
                              (apply format format-string format-args)
                              (parse-tree->location-string causal-parse-tree))
                      (current-continuation-marks))))

(struct exn:fail:juhz exn:fail () #:transparent)

(struct exn:fail:juhz:internal-error exn:fail:juhz () #:transparent
  #:extra-constructor-name make-exn:fail:juhz:internal-error)

(define report/internal-error (report make-exn:fail:juhz:internal-error))

(struct exn:fail:juhz:unbound-variable exn:fail:juhz () #:transparent
  #:extra-constructor-name make-exn:fail:juhz:unbound-variable)

(define report/unbound-variable (report make-exn:fail:juhz:unbound-variable))

(struct exn:fail:juhz:illegal-operation exn:fail:juhz () #:transparent
  #:extra-constructor-name make-exn:fail:juhz:illegal-operation)

(define report/illegal-operation (report make-exn:fail:juhz:illegal-operation))

(define ast%
  (class object%
    (init-field [parse-tree #f])
    (abstract evaluate)
    (super-new)))

(define constant%
  (class ast%
    (init-field constant-token)
    (super-new)
    (field [value
            (match constant-token
              [(struct token ('NUMBER text _))
               (make-object/NUMBER (string->number text))]
              [(struct token ('STRING text _))
               (make-object/STRING+escape text)]
              [(struct token ('TRUE _ _))
               (make-object/BOOLEAN true)]
              [(struct token ('FALSE _ _))
               (make-object/BOOLEAN false)]
              [else (report/internal-error constant-token "~A is not a constant" constant-token)])])
    (define/override (evaluate package/env)
      (result value package/env #f))))

(define condition%
  (class ast%
    (init-field cond-ast true-case-ast false-case-ast)
    (super-new)
    (define/override (evaluate package/env)
      (define cond-object (~> (send cond-ast evaluate package/env) result-object))
      (cond [(and (object-true? cond-object) (eq? true-case-ast 'same-as-condition)) (result cond-object package/env #f)]
            [(object-true? cond-object) (send true-case-ast evaluate (extend-package package/env))]
            [(object? false-case-ast) (result false-case-ast package/env #f)]
            [else (send false-case-ast evaluate (extend-package package/env))]))))

(define loop%
  (class ast%
    (init-field cond-ast body-ast)
    (super-new)
    (define/override (evaluate package/env)
      (let ([last-value (make-object/BOOLEAN false)])
        (let continue ([cond-object (~> (send cond-ast evaluate package/env) result-object)])
          (cond [(object-true? cond-object)
                 (set! last-value (send body-ast evaluate (extend-package package/env)))
                 (continue (~> (send cond-ast evaluate package/env) result-object))]
                [else (result last-value package/env #f)]))))))

(define package%
  (class ast%
    (init-field public-name-list body-ast/collector-program)
    (super-new)
    (define/override (evaluate package/env)
      (define package
        (~> (send body-ast/collector-program evaluate (extend-package package/env))
            result-reserved-data))
      (cond [(not public-name-list) (result (make-object/PACKAGE package) package/env #f)]
            [else (for/fold ([package/interface (extend-package root-package)]
                             #:result (result (make-object/PACKAGE package/interface) package/env #f))
                            ([name (in-list public-name-list)])
                    (define value (find-in-package package))
                    (if (not value)
                        (report/unbound-variable (get-field parse-tree this) "Cannot export unbound symbol ~A from package" name)
                        (define-in-package! package/interface name value)))]))))

(define function%
  (class ast%
    (init-field argument-name-list body-ast/basic-program)
    (super-new)
    (define/override (evaluate package/env)
      (result (make-object/FUNCTION argument-name-list body-ast/basic-program package/env)
              package/env #f))))

(define array%
  (class ast%
    (init-field ast-list)
    (super-new)
    (define/override (evaluate package/env)
      (define array (~>> ast-list
                         (map (lambda~> (send evaluate package/env) result-object))
                         list->vector))
      (result (make-object/ARRAY array) package/env #f))))

(define identifier%
  (class ast%
    (init-field name)
    (super-new)
    (define/override (evaluate package/env)
      (define object (find-in-package package/env name))
      (if object
          (result object package/env #f)
          (report/unbound-variable (get-field parse-tree this)
                                   "Cannot find ~A in current environment" name)))))

(define program%
  (class ast%
    (init-field statement-ast-list)
    (super-new)
    (define/override (evaluate package/env)
      (for/fold ([last-value (make-object/BOOLEAN #f)]
                 [package/env* package/env]
                 #:result (result last-value package/env* #f))
                ([statement-ast (in-list statement-ast-list)])
        (let ([result (send statement-ast evaluate package/env*)])
          (values (result-object result)
                  (result-package/env result)))))))

(define collector-program%
  (class ast%
    (init-field statement-ast-list)
    (super-new)
    (define/override (evaluate package/env)
      (match-define (struct package (direct-mapping linked-package-list))
        (for/fold ([package/env* (extend-package package/env)]
                   #:result package/env*)
                  ([statement-ast (in-list statement-ast-list)])
          (~> statement-ast (send evaluate package/env*) result-package/env)))
      (define linked-package-list/new
        (~> (take linked-package-list (- (length linked-package-list)
                                         (length (package-linked-package-list package/env))))
            (append (list root-package))))
      (result #f package/env (package direct-mapping linked-package-list/new)))))

(define use%
  (class ast%
    (init-field expression-ast)
    (super-new)
    (define/override (evaluate package/env)
      (define used-package (~> expression-ast (send evaluate package/env) result-object object-package))
      (result (make-object/BOOLEAN #f) (using-package package/env used-package) #f))))

(define definition%
  (class ast%
    (init-field name value-ast)
    (super-new)
    (define/override (evaluate package/env)
      (define value-object (~> value-ast (send evaluate package/env) result-object))
      (result (make-object/BOOLEAN #f) (define-in-package! package/env name value-object) #f))))

(define function-definition%
  (class ast%
    (init-field name argument-name-list body-ast/basic-program)
    (super-new)
    (define/override (evaluate package/env)
      (define function-ast
        (new function%
             (argument-name-list argument-name-list)
             (body-ast/basic-program body-ast/basic-program)))
      (define declarative-package/env
        (define-in-package! package/env name (make-object/BOOLEAN #f)))
      (define function-object (~> function-ast (send evaluate declarative-package/env) result-object))
      (modify-in-package! declarative-package/env name function-object)
      (result (make-object/BOOLEAN #f) declarative-package/env #f))))

(define package-definition%
  (class ast%
    (init-field name value-ast)
    (super-new)
    (define/override (evaluate package/env)
      (library-package-set! name (~> (send value-ast evaluate package/env) result-object))
      (result (make-object/BOOLEAN #f) package/env #f))))

(define assignment%
  (class ast%
    (init-field name value-ast)
    (super-new)
    (define/override (evaluate package/env)
      (define value-object (~> value-ast (send evaluate package/env) result-object))
      (define package/env+
        (or (and (modify-in-package! package/env name value-object)
                 package/env)
            (define-in-package! package/env name value-object)))
      (result (make-object/BOOLEAN #f) package/env+ #f))))

(define selection-assignment%
  (class ast%
    (init-field prefix-ast name value-ast)
    (super-new)
    (define/override (evaluate package/env)
      (define value-object (~> value-ast (send evaluate package/env) result-object))
      (define prefix-object (~> prefix-ast (send evaluate package/env) result-object))
      (or (modify-in-package! (object-package prefix-object) name value-object)
          (set-object-package! prefix-object
                               (define-in-package! (object-package prefix-object) name value-object)))
      (result (make-object/BOOLEAN #f) package/env #f))))

(define package-assignment%
  (class ast%
    (init-field name value-ast)
    (super-new)
    (define/override (evaluate package/env)
      (library-package-set! name (~> (send value-ast evaluate package/env) result-object))
      (result (make-object/BOOLEAN #f) package/env #f))))

(define (bind-argument function argument-object-list)
  (match-define
    (struct object ('function
                    (struct object/function
                      (argument-name-list body-ast package/env))
                    _))
    function)
  (for/fold ([middle-package/env package/env])
            ([name (in-list argument-name-list)]
             [object (sequence-append (in-list argument-object-list)
                                      (in-cycle (stream object/NOT-PROVIDED)))])
    (define-in-package! middle-package/env name object)))

(define (juhz-apply function-like middle-package-modifier . arguments)
  (define argument-object-list (apply list* arguments))
  (cond [(procedure? function-like)
         (function-like argument-object-list)]
        [(eq? 'function (object-type function-like))
         (define body-ast (~> function-like object-value object/function-body-ast))
         (define argument-bound-package/env
           (bind-argument function-like argument-object-list))
         (result-object
          (send body-ast evaluate (middle-package-modifier argument-bound-package/env)))]
        [else
         (define applicable (find-field function-like "__APPLY__"))
         (juhz-apply applicable middle-package-modifier argument-object-list)]))

(define call%
  (class ast%
    (init-field caller-ast argument-ast-list suffix-ast suffix-type)
    (super-new)
    (define/override (evaluate package/env)
      (define caller-object (~> caller-ast (send evaluate package/env) result-object))
      (unless (or (procedure? caller-object) (eq? 'function (object-type caller-object))
                  (find-field caller-object "__APPLY__"))
        (report/illegal-operation (get-field parse-tree caller-ast) "The object cannot be used as a function"))
      (define argument-object-list
        (map (lambda~> (send evaluate package/env) result-object) argument-ast-list))
      (define (juhz-apply* . args)
        (result (apply juhz-apply args) package/env #f))
      (match suffix-type
        [#f (juhz-apply* caller-object identity argument-object-list)]
        ['normal
         (define wrapper
           (new function%
                (argument-name-list null)
                (body-ast/basic-program suffix-ast)
                (parse-tree (get-field parse-tree suffix-ast))))
         (define wrapper-object
           (~> (send wrapper evaluate package/env)
               result-object))
         (juhz-apply* caller-object identity (append argument-object-list (list wrapper-object)))]
        ['function
         (define function-object
           (~> (send suffix-ast evaluate package/env) result-object))
         (juhz-apply* caller-object identity (append argument-object-list (list function-object)))]
        ['package
         (define base-package
           (~> suffix-ast (send evaluate package/env) result-object object-package))
         (juhz-apply* caller-object (lambda~>> (using-package base-package)) argument-object-list)]))))

(define selection%
  (class ast%
    (init-field target-ast property-name)
    (super-new)
    (define/override (evaluate package/env)
      (define target-package (~> target-ast (send evaluate package/env) result-object object-package))
      (define property-object (find-in-package target-package property-name))
      (if property-object
          (result property-object package/env #f)
          (report/unbound-variable (get-field parse-tree this) "Cannot found property ~A in the package" property-name)))))

(define package-selection%
  (class ast%
    (init-field name)
    (super-new)
    (define/override (evaluate package/env)
      (define object (hash-ref library-package-table name #f))
      (if object
          (result object package/env #f)
          (report/unbound-variable (get-field parse-tree this) "Package ~A has not been defined yet" name)))))

(provide (all-defined-out))
