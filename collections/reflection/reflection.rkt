#lang racket

(require threading)
(require juhz/api)

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
         make-object/ARRAY))
  (def TC_BOOLEAN (make-object/NUMBER 0))
  (def TC_NUMBER (make-object/NUMBER 1))
  (def TC_STRING (make-object/NUMBER 2))
  (def TC_ARRAY (make-object/NUMBER 3))
  (def TC_FUNCTION (make-object/NUMBER 4))
  (def TC_PROCEDURE (make-object/NUMBER 5))
  (def TC_PACKAGE (make-object/NUMBER 6))
  (def TC_INTERNAL (make-object/NUMBER 7))
  (def (typeCode object)
    (if (procedure? object)
        (make-object/NUMBER 5)
        (type-case "typeCode" (object)
                   [(boolean) (make-object/NUMBER 0)]
                   [(number) (make-object/NUMBER 1)]
                   [(string) (make-object/NUMBER 2)]
                   [(array) (make-object/NUMBER 3)]
                   [(function) (make-object/NUMBER 4)]
                   [(package) (make-object/NUMBER 6)]
                   [else (make-object/NUMBER 7)]))))
