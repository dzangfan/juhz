#lang racket

(require threading)
(require "runtime.rkt")
(require "interpreter.rkt")

(define-library-package builtin
  (def (length array/string)
    (type-case "length"
               (array/string)
               [(array)
                (make-object/NUMBER
                 (~> array/string object-value vector-length))]
               [(string)
                (make-object/NUMBER
                 (~> array/string object-value string-length))]))
  (def NOT_PROVIDED object/NOT-PROVIDED)
  (def UNDEFINED object/UNDEFINED)
  (def (push elt array)
    (type-case "push"
               (array)
               [(array)
                (set-object-value! array (vector-append (object-value array)
                                                        (vector elt)))
                array])))
