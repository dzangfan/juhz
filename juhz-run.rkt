#!/usr/bin/env racket
#lang racket

(require racket/cmdline)

(define run-stdin? (make-parameter #f))

(define (get-source-files [argv (current-command-line-arguments)])
  (command-line #:program "juhz-run" #:argv argv
                #:once-each (("-i" "--stdin") "Evaluate code in STDIN after runing sources from arguments"
                                              (run-stdin? #t))
                #:usage-help "Run source files specified by arguments sequentially."
                #:args source-files
                source-files))

(module+ main
  (require threading)
  (require racket/runtime-path)
  (require "interpreter.rkt")
  (require "runtime.rkt")
  (require "base-operations.rkt")
  (require "builtin.rkt")

  (define-runtime-path juhz-collection-path "collections")

  (with-handlers
    ([exn:fail:juhz?
      (lambda (e)
        (displayln "++ JUHZ ERROR ++")
        (displayln (exn-message e)))]
     [exn:fail:filesystem?
      (lambda (e)
        (displayln "++ IO ERROR ++")
        (displayln (exn-message e)))]
     [exn:fail? (lambda (e)
                  (displayln "++ INTERNAL ERROR ++")
                  (raise e))])
    
    (void (source (build-path juhz-collection-path ".all.juhz")))

    (define source-files (get-source-files))

    (for ([file (in-list source-files)])
      (juhz-eval (open-input-file file) (extend-package root-package) #:file file))

    (when (run-stdin?)
      (void (juhz-eval (current-input-port) (extend-package root-package))))))
