#lang racket

(require threading)
(require "runtime.rkt")
(require "interpreter.rkt")

(define current-source-file (make-parameter #f))

(define (ensure-filename filename)
  (cond [(current-source-file)
         (~> filename (path->complete-path (path-only (current-source-file))) resolve-path)]
        [(absolute-path? filename)
         (resolve-path filename)]
        [else (report/runtime-error "Cannot resolve [~A] as the name of a source file." filename)]))

(define (source filename)
  (define path (ensure-filename filename))
  (if (file-exists? path)
      (let ([in (open-input-file path)])
        (parameterize ([current-source-file path])
          (juhz-load in #:file (path->string path))))
      object/UNDEFINED))

(define (load-racket filename)
  (define path (ensure-filename filename))
  (if (file-exists? path)
      (dynamic-require path #f)
      (report/runtime-error "Cannot load [~A] as a racket resources" path))
  (make-object/BOOLEAN #f))

(provide source load-racket)

(define-library-package builtin
  (def (source filename)
    (type-case "source" (filename)
               [(string) (source (object-value filename))]))
  (def (load filename)
    (type-case "load" (filename)
               [(string) (load-racket (object-value filename))]))
  (def NOT_PROVIDED object/NOT-PROVIDED)
  (def UNDEFINED object/UNDEFINED))
