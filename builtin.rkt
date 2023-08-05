#lang racket

(require threading)
(require "runtime.rkt")
(require "interpreter.rkt")

(define current-source-file (make-parameter #f))

(define (source filename)
  (define path
    (cond [(current-source-file)
           (~> filename (path->complete-path (path-only (current-source-file))) resolve-path)]
          [(absolute-path? filename)
           (resolve-path filename)]
          [else (report/runtime-error "Cannot resolve [~A] as the name of a source file." filename)]))
  (if (file-exists? path)
      (let ([in (open-input-file path)])
        (parameterize ([current-source-file path])
          (juhz-load in #:file (path->string path))))
      object/UNDEFINED))

(provide source)

(define-library-package builtin
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
                  (make-object/BOOLEAN #f))]))
  (def (source filename)
    (type-case "source" (filename)
               [(string) (source (object-value filename))]))
  (def NOT_PROVIDED object/NOT-PROVIDED)
  (def UNDEFINED object/UNDEFINED))
