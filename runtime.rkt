#lang racket

(require threading)
(require "compiler.rkt")
(require "interpreter.rkt")

(define (juhz-eval in #:file [file "(string)"])
  (define program (juhz-compile in #:file file))
  (send program evaluate (extend-package root-package)))

(define (juhz-load in #:file [file "(string)"])
  (define program (juhz-compile in #:file file))
  (define collector-program
    (new collector-program%
         (statement-ast-list (get-field statement-ast-list program))
         (parse-tree (get-field parse-tree program))))
  (define package
    (new package%
         (public-name-list #f)
         (body-ast/collector-program collector-program)
         (parse-tree (get-field parse-tree collector-program))))
  (~> (send package evaluate (extend-package root-package))
      result-object))

(struct exn:fail:juhz:runtime-error exn:fail:juhz () #:transparent
  #:extra-constructor-name make-exn:fail:juhz:runtime-error)

(define (report/runtime-error format-string . format-args)
  (raise
   (make-exn:fail:juhz:runtime-error
    (apply format format-string format-args)
    (current-continuation-marks))))

(define (make-standard-internal-function number-of-argument procedure)
  (lambda (argument-ast-list suffix-ast suffix-type package/env)
    (when suffix-type
      (report/illegal-operation suffix-ast "Standard internal functions cannot process functions with suffixes"))
    (define argument-object-list
      (for/list ([argument-ast (in-list argument-ast-list)])
        (result-object (send argument-ast evaluate package/env))))
    (define gap (- number-of-argument (length argument-object-list)))
    (cond [(zero? gap) (apply procedure argument-object-list)]
          [(positive? gap)
           (define argument-object-list+
             (append argument-object-list
                     (make-list gap object/NOT-PROVIDED)))
           (apply procedure argument-object-list+)]
          [(negative? gap)
           (define argument-object-list-
             (take number-of-argument argument-object-list))
           (apply procedure argument-object-list-)])))

(define-syntax-rule (define-base-function base-package/* function-name (argument-list ...) body ...)
  (set-box! base-package/*
            (define-in-package (unbox base-package/*) function-name
              (make-standard-internal-function (length '(argument-list ...))
                                               (lambda (argument-list ...) body ...)))))

(define-syntax type-case-helper
  (syntax-rules (else)
    [(_ operation-name (object ...) #:collect (match-clauses ...) #:clause ())
     (let ([type-list (map object-type (list object ...))])
       (match type-list
         match-clauses ...
         [else (report/runtime-error "Illegal operation: ~A(~A)" operation-name
                                     (string-join (map symbol->string type-list) ", "))]))]
    [(_ operation-name (object ...)
        #:collect (match-clauses ...)
        #:clause ([(type-symbols ...) body ...] rest-clauses ...))
     (type-case-helper operation-name
                       (object ...)
                       #:collect (match-clauses ... [(list 'type-symbols ...) body ...])
                       #:clause (rest-clauses ...))]
    [(_ operation-name (object ...)
        #:collect (match-clauses ...)
        #:clause ([else body ...]))
     (match (map object-type (list object ...))
       match-clauses ... [else body ...])]))

(define-syntax-rule (type-case operation-name (object ...) clauses ...)
  (type-case-helper operation-name (object ...) #:collect () #:clause (clauses ...)))

(define-base-function base-package/NUMBER "__PLUS__" (left-number right-number)
  (type-case "number.__PLUS__"
             (left-number right-number)
             [(number number)
              (make-object/NUMBER (+ (object-value left-number) (object-value right-number)))]
             [(number not-provided)
              (make-object/NUMBER (object-value left-number))]))

(provide juhz-eval juhz-load)
