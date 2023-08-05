#lang racket

(require threading)
(require "runtime.rkt")
(require "interpreter.rkt")

(define-library-package builtin
  (def (length array/string maybe-length)
    (type-case "length"
               (array/string maybe-length)
               [(array not-provided)
                (make-object/NUMBER
                 (~> array/string object-value vector-length))]
               [(array number)
                (let ([length+ (object-value maybe-length)]
                      [vector (object-value array/string)])
                  (set-object-value!
                   array/string
                   (if (<= length+ (vector-length vector))
                       (vector-take vector length+)
                       (vector-append vector
                                      (make-vector (- length+ (vector-length vector))
                                                   object/NOT-PROVIDED))))
                  (make-object/BOOLEAN #f))]))
  (def NOT_PROVIDED object/NOT-PROVIDED)
  (def UNDEFINED object/UNDEFINED))
