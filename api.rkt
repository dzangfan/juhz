#lang racket

(require "interpreter.rkt")
(require "runtime.rkt")

(provide (struct-out package)
         (struct-out object)
         (struct-out object/function)
         (struct-out result))

(define storable/c (or/c procedure? object?))

(define function-like/c (or/c procedure? object?))

(provide storable/c function-like/c)

(provide
 (contract-out (no-more-package null)
               (root-package package?)
               (extend-package (-> package? package?))
               (using-package (-> package? package? package?))
               (find-in-package (-> package? string? (or/c #f storable/c)))
               (find-field (-> object? string? (or/c #f storable/c)))
               (define-in-package (-> package? string? storable/c package?))
               (modify-in-package! (-> package? string? storable/c boolean?))
               (modify-root-package! (-> string? storable/c any/c))))

(provide
 (contract-out (object/NOT-PROVIDED object?)
               (object/UNDEFINED object?)
               (make-object/NUMBER (-> number? object?))
               (make-object/BOOLEAN (-> boolean? object?))
               (make-object/STRING (-> string? object?))
               (make-object/PACKAGE (-> package? object?))
               (make-object/ARRAY (-> vector? object?))
               (object-true? (-> storable/c boolean?))))

(provide
 (contract-out (library-package-set! (-> string? object? any/c))
               (library-package-ref (-> string? (or/c object? #f)))))

(provide
 (contract-out (juhz-apply (-> function-like/c (-> package? package?) list? storable/c))
               (juhz-eval (->* ((or/c string? input-port?)) (package? #:file string?) result?))
               (juhz-load (->* ((or/c string? input-port?)) (#:file string?) object?))))

(provide define-library-package type-case)
