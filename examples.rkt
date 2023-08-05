#lang racket

(module+ test
  (require threading)
  (require rackunit)
  (require "interpreter.rkt")
  (require "runtime.rkt")
  (require "base-operations.rkt")
  (require "builtin.rkt")
  (define (eval string)
    (define base-package (extend-package root-package))
    (source (build-path (current-directory) "collections" ".all.juhz"))
    (juhz-eval string base-package))
  (define (racketify any)
    (match any
      [(struct object (type value _))
       #:when (memq type '(boolean number string))
       value]
      [(struct object ('array value _))
       (vector-map racketify value)]
      [compound-value compound-value]))
  (define (eval/value string)
    (~> (eval string) result-object racketify)))
