#lang racket

(require "runtime.rkt")
(require "interpreter.rkt")

(define (implement-logical-not base-package/*)
  (define-base-function base-package/* "__BANG__" (self)
    (if (object-true? self)
        (make-object/BOOLEAN #f)
        (make-object/BOOLEAN #t))))

(implement-logical-not base-package/ARRAY)
(implement-logical-not base-package/BOOLEAN)
(implement-logical-not base-package/FUNCTION)
(implement-logical-not base-package/NUMBER)
(implement-logical-not base-package/STRING)

(define (implement-naive-equality base-package/*)
  (define (naive-equal? self other)
    (and (eq? (object-type self) (object-type other))
         (equal? (object-value self) (object-value other))))
  (define-base-function base-package/* "__SAME__" (self other)
    (make-object/BOOLEAN (naive-equal? self other)))
  (define-base-function base-package/* "__DIFF__" (self other)
    (make-object/BOOLEAN (not (naive-equal? self other)))))

(implement-naive-equality base-package/ARRAY)
(implement-naive-equality base-package/BOOLEAN)
(implement-naive-equality base-package/NUMBER)
(implement-naive-equality base-package/STRING)

(define-base-function base-package/FUNCTION "__SAME__" ()
  (make-object/BOOLEAN #f))
(define-base-function base-package/FUNCTION "__DIFF__" ()
  (make-object/BOOLEAN #f))

;; -------------------- number --------------------

(define-base-function base-package/NUMBER "__PLUS__" (left-number right-number)
  (type-case "number.__PLUS__"
             (left-number right-number)
             [(number number)
              (make-object/NUMBER (+ (object-value left-number) (object-value right-number)))]
             [(number not-provided)
              (make-object/NUMBER (object-value left-number))]))

(define-base-function base-package/NUMBER "__MINUS__" (left-number right-number)
  (type-case "number.__MINUS__"
             (left-number right-number)
             [(number number)
              (make-object/NUMBER (- (object-value left-number) (object-value right-number)))]
             [(number not-provided)
              (make-object/NUMBER (- (object-value left-number)))]))

(define (link-number-operation function hook-name)
  (define-base-function base-package/NUMBER hook-name (left-number right-number)
    (type-case (format "number.~A" hook-name)
               (left-number right-number)
               [(number number)
                (define result
                  (function (object-value left-number)
                            (object-value right-number)))
                (define constructor (if (number? result) make-object/NUMBER make-object/BOOLEAN))
                (constructor result)])))

(link-number-operation < "__LT__")
(link-number-operation > "__GT__")
(link-number-operation <= "__LE__")
(link-number-operation >= "__GE__")
(link-number-operation * "__TIMES__")
(link-number-operation / "__DIVIDE__")
(link-number-operation remainder "__REM__")

;; -------------------- string --------------------

(define-base-function base-package/STRING "__PLUS__" (left-string right-string)
  (type-case "string.__PLUS__"
             (left-string right-string)
             [(string string)
              (make-object/STRING (string-append (object-value left-string)
                                                 (object-value right-string)))]))

;; -------------------- array --------------------

(define (valid-index? vector number)
  (and (integer? number)
       (<= 0 number)
       (< number (vector-length vector))))

(define-base-function base-package/ARRAY "__INDEX__" (array index new-value)
  (type-case "array.__INDEX__"
             (array index new-value)
             [(array number not-provided)
              (let ([index (object-value index)]
                    [vector (object-value array)])
                (if (valid-index? vector index)
                    (vector-ref vector index)
                    object/UNDEFINED))]
             [else (type-case "array.__INDEX__"
                              (array index)
                              [(array number)
                               (let ([index (object-value index)]
                                     [vector (object-value array)])
                                 (if (valid-index? vector index)
                                     (begin (vector-set! vector index new-value)
                                            new-value)
                                     object/UNDEFINED))])]))
