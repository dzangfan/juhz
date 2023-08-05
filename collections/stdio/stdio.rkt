#lang racket

(require juhz/runtime)
(require juhz/interpreter)

(define-library-package stdio_
  (def (print string)
    (type-case "stdio_.print" (string)
               [(string) (display (object-value string))]))
  (def (flush)
    (flush-output)))
