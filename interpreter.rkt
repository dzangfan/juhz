#lang racket

(require basic-cc/tokenization)
(require threading)

(struct package (direct-mapping linked-package-list) #:transparent)

(define root-package (package #hash() null))

(define (extend-package parent-package)
  (package #hash() (list parent-package)))

(define (using-package current-package used-package)
  (match-define (struct package (direct-mapping linked-package-list)) current-package)
  (package direct-mapping (cons used-package linked-package-list)))

(define (find-in-package current-package name)
  (cond [(eq? current-package root-package) #f]
        [else (match-define (struct package (direct-mapping linked-package-list)) current-package)
              (if (hash-has-key? direct-mapping name)
                  (hash-ref direct-mapping name)
                  (for/first ([used-package (in-list linked-package-list)]
                              #:do [(define result (find-in-package used-package))]
                              #:when result)
                    result))]))

(define (modify-in-package current-package name value)
  (cond [(eq? current-package root-package) #f]
        [else (match-define (struct package (direct-mapping linked-package-list)) current-package)
              (if (hash-has-key? direct-mapping name)
                  (package (hash-set direct-mapping name value) linked-package-list)
                  (let try-in-linked-packages ([rest-packages linked-package-list] [tried-packages/rev null])
                    (match rest-packages
                      [(list) #f]
                      [(list used-package rest-packages+ ...)
                       (define used-package/modified (modify-in-package used-package name value))
                       (if used-package/modified
                           (package direct-mapping
                                    (for/fold ([linked-package-list+ (cons used-package/modified rest-packages+)])
                                              ([tried-package (in-list tried-packages/rev)])
                                      (cons tried-package linked-package-list+)))
                           (try-in-linked-packages rest-packages+ (cons used-package tried-packages/rev)))])))]))

(define (define-in-package current-package name value)
  (match-define (struct package (direct-mapping linked-package-list)) current-package)
  (package (hash-set direct-mapping name value)
           linked-package-list))

(struct object/function (argument-name-list body-ast package/env) #:transparent)

(struct object (type value package) #:transparent)

(struct result (object package/env reserved-data) #:transparent)

(define (make-object/NUMBER number)
  (object 'number number (extend-package root-package)))

(define (make-object/STRING string)
  (let ([content (substring string 1 (sub1 (string-length string)))])
    (object 'string content (extend-package root-package))))

(define (make-object/BOOLEAN true?)
  (object 'boolean true? (extend-package root-package)))

(define (object-true? any-object)
  (match any-object
    [(struct object ('boolean #false _)) #f]
    [else #t]))

(define (make-object/PACKAGE package)
  (object 'package #f package))

(define (make-object/FUNCTION argument-name-list body-ast package/env)
  (object 'function (object/function argument-name-list body-ast package/env)
          (extend-package root-package)))

(define (make-object/ARRAY vector)
  (object 'array vector (extend-package root-package)))

(define object/NOT-PROVIDED
  (object 'not-provided #f (extend-package root-package)))

(define ((report constructor) causal-parse-tree format-string . format-args)
  (raise (constructor (apply format format-string format-args)
                      (current-continuation-marks))))

(struct exn:fail:juhz exn:fail () #:transparent)

(struct exn:fail:juhz:internal-error exn:fail:juhz () #:transparent
  #:extra-constructor-name make-exn:fail:juhz:internal-error)

(define report/internal-error (report make-exn:fail:juhz:internal-error))

(struct exn:fail:juhz:unbound-variable exn:fail:juhz () #:transparent
  #:extra-constructor-name make-exn:fail:juhz:unbound-variable)

(define report/unbound-variable (report make-exn:fail:juhz:unbound-variable))

(define ast%
  (class object%
    (init-field [parse-tree #f])
    (abstract evaluate)
    (super-new)))

(define constant%
  (class ast%
    (init-field constant-token)
    (super-new (parse-tree constant-token))
    (field [value
            (match constant-token
              [(struct token ('NUMBER text _))
               (make-object/NUMBER (string->number text))]
              [(struct token ('STRING text _))
               (make-object/STRING text)]
              [(struct token ('TRUE _ _))
               (make-object/BOOLEAN true)]
              [(struct token ('FALSE _ _))
               (make-object/BOOLEAN false)]
              [else (report/internal-error constant-token "~A is not a constant" constant-token)])])
    (define/override (evaluate package/env)
      (result value package/env #f))))

(define condition%
  (class ast%
    (init-field cond-ast true-case-ast false-case-ast parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (define cond-object (~> (send cond-ast evaluate package/env) result-object))
      (cond [(and (object-true? cond-object) (eq? true-case-ast 'same-as-condition)) (result cond-object package/env #f)]
            [(object-true? cond-object) (send true-case-ast evaluate (extend-package package/env))]
            [else (send false-case-ast evaluate (extend-package package/env))]))))

(define loop%
  (class ast%
    (init-field cond-ast body-ast parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (let ([last-value (make-object/BOOLEAN false)])
        (let continue ([cond-object (~> (send cond-ast evaluate package/env) result-object)])
          (if (object-true? cond-object)
              (set! last-value (send body-ast evaluate (extend-package package/env)))
              last-value))))))

(define package%
  (class ast%
    (init-field public-name-list body-ast/collector-program parse-tree)
    (super-new (parse-tree parse-tree))
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
                        (report/unbound-variable parse-tree "Cannot export unbound symbol ~A from package" name)
                        (define-in-package package/interface name value)))]))))

(define function%
  (class ast%
    (init-field argument-name-list body-ast/basic-program parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (result (make-object/FUNCTION argument-name-list body-ast/basic-program package/env)
              package/env #f))))

(define array%
  (class ast%
    (init-field ast-list parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (define array (~>> ast-list
                         (map (lambda~> (send evaluate package/env) result-object))
                         list->vector))
      (result (make-object/ARRAY array) package/env #f))))

(define indentifier%
  (class ast%
    (init-field name token)
    (super-new (parse-tree token))
    (define/override (evaluate package/env)
      (define object (find-in-package package/env name))
      (if object
          (result object package/env #f)
          (report/unbound-variable token "Cannot find ~A in current environment" name)))))

(define program%
  (class ast%
    (init-field statement-ast-list parse-tree)
    (super-new (parse-tree parse-tree))
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
    (init-field statement-ast-list parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (match-define (struct package (direct-mapping linked-package-list))
        (for/fold ([package/env* (extend-package package/env)]
                   #:result package/env*)
                  ([statement-ast (in-list statement-ast-list)])
          (~> statement-ast (send evaluate package/env*) result-package/env)))
      (define linked-package-list/new
        (take linked-package-list (- (length linked-package-list) (length package/env))))
      (result #f package/env (package direct-mapping linked-package-list/new)))))

(define use%
  (class ast%
    (init-field expression-ast parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (define used-package (~> expression-ast (send evaluate package/env) result-object object-package))
      (result (make-object/BOOLEAN #f) (using-package package/env used-package) #f))))

(define definition%
  (class ast%
    (init-field name value-ast parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (define value-object (~> value-ast (send evaluate package/env) result-object))
      (result (make-object/BOOLEAN #f) (define-in-package package/env name value-object) #f))))

(define assignment%
  (class ast%
    (init-field name value-ast parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (define value-object (~> value-ast (send evaluate package/env) result-object))
      (define package/env+
        (or (modify-in-package package/env name value-object)
            (define-in-package package/env name value-object)))
      (result (make-object/BOOLEAN #f) package/env+ #f))))

(define (bind-argument function argument-object-list)
  (match-define
    (struct object ('function
                    (struct object/function
                      (argument-name-list body-ast package/env))
                    _))
    function)
  (for/fold ([middle-package/env package/env])
            ([name (in-list argument-name-list)]
             [object (stream-append (in-list argument-object-list)
                                    (in-cycle (stream object/NOT-PROVIDED)))])
    (define-in-package middle-package/env name object)))

(define call%
  (class ast%
    (init-field caller-ast argument-ast-list suffix-ast suffix-type parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (define caller-object (~> caller-ast (send evaluate package/env) result-object))
      (cond [(procedure? caller-object)
             (result (caller-object argument-ast-list suffix-ast suffix-type package/env)
                     package/env #f)]
            [else
             (define (apply argument-ast-list middle-package-modifier)
               (define argument-object-list
                 (map (lambda~> (send evaluate package/env) result-object) argument-ast-list))
               (define body-ast (~> caller-object object-value object/function-body-ast))
               (define argument-bound-package/env
                 (bind-argument caller-object argument-object-list))
               (result (send body-ast evaluate (middle-package-modifier argument-bound-package/env))
                       package/env #f))
             (match suffix-type
               [#f (apply argument-ast-list identity)]
               ['normal
                (define wrapper
                  (new function%
                       (argument-name-list null)
                       (body-ast/basic-program suffix-ast)
                       (parse-tree (get-field parse-tree suffix-ast))))
                (apply (append argument-ast-list (list wrapper)) identity)]
               ['function
                (apply (append argument-ast-list (list suffix-ast)) identity)]
               ['package
                (define base-package
                  (~> suffix-ast (send evaluate package/env) object-package))
                (apply argument-ast-list (lambda~>> (using-package base-package)))])]))))

(define selection%
  (class ast%
    (init-field target-ast property-name parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (define target-package (~> target-ast (send evaluate package/env) result-object object-package))
      (define property-object (find-in-package target-package property-name))
      (if property-object
          (result property-object package/env #f)
          (report/unbound-variable parse-tree "Cannot found property ~A in the package" property-name)))))

(define library-package (make-object/PACKAGE (extend-package root-package)))

(define (update-package name-list result-object)
  (set! library-package
        (let update ([rest-name-list name-list] [package-object library-package])
          (match-define (struct object ('package #f package)) package-object)
          (match rest-name-list
            [(list name) (make-object/PACKAGE (define-in-package package name result-object))]
            [(list name rest-name-list+ ...)
             (define next-package-object (find-in-package package))
             (make-object/PACKAGE
              (define-in-package package name
                (update rest-name-list+
                        (or next-package-object
                            (make-object/PACKAGE (extend-package root-package))))))]))))

(define package-selection%
  (class ast%
    (init-field name parse-tree)
    (super-new (parse-tree parse-tree))
    (define/override (evaluate package/env)
      (define object (~> library-package object-package (find-in-package name)))
      (if object
          (result object package/env #f)
          (report/unbound-variable parse-tree "Package ~A has not been defined yet" name)))))

(provide (all-defined-out))
