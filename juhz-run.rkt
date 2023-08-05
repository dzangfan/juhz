#!/usr/bin/env racket
#lang racket

(require racket/cmdline)

(define (get-source-files [argv (current-command-line-arguments)])
  (command-line #:program "juhz-run" #:argv argv
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
  
  (void (source (build-path juhz-collection-path ".all.juhz")))

  (define source-files (get-source-files))

  (for ([file (in-list source-files)])
    (juhz-eval (open-input-file file) (extend-package root-package))))
