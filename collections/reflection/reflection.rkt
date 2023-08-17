#lang racket

(require threading)
(require juhz/interpreter)
(require juhz/runtime)

(define-library-package reflection_
  (def (packageField pkg fld val)
    (type-case "packageField" (fld val)
               [(string not-provided)
                (or (find-in-package (object-package pkg) (object-value fld))
                    object/UNDEFINED)]
               [else (or (modify-in-package! (object-package pkg) (object-value fld) val)
                         (set-object-package! pkg (define-in-package (object-package pkg)
                                                    (object-value fld) val)))
                     (make-object/BOOLEAN #f)]))
  (def (definePackageField pkg fld val)
    (set-object-package! pkg
                         (define-in-package (object-package pkg)
                           (object-value fld) val))
    (make-object/BOOLEAN #f))
  (def (usePackage pkg used-pkg)
    (set-object-package! pkg (using-package (object-package pkg) (object-package used-pkg)))
    (make-object/BOOLEAN #f))
  (def (packageFields pkg)
    (~>> pkg object-package package-direct-mapping hash-keys
         (map make-object/STRING)
         list->vector
         make-object/ARRAY)))
