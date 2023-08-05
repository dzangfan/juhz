#lang racket

(require threading)
(require juhz/runtime)
(require juhz/interpreter)

(define-library-package array_
  (def (length array maybe-length)
    (type-case "length"
               (array maybe-length)
               [(array not-provided)
                (make-object/NUMBER
                 (~> array object-value vector-length))]
               [(array number)
                (let ([length+ (object-value maybe-length)]
                      [vector (object-value array)])
                  (set-object-value!
                   array
                   (if (<= length+ (vector-length vector))
                       (vector-take vector length+)
                       (vector-append vector
                                      (make-vector (- length+ (vector-length vector))
                                                   object/NOT-PROVIDED))))
                  (make-object/BOOLEAN #f))])))
