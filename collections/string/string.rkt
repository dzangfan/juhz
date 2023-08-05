#lang racket

(require threading)
(require juhz/runtime)
(require juhz/interpreter)

(define (take n lst)
  (let take* ([n n] [lst lst] [rez null])
    (if (or (zero? n) (null? lst))
        (values (reverse rez) (null? lst))
        (take* (sub1 n) (rest lst) (cons (first lst) rez)))))

(define (limitied-list n lst)
  (let-values ([(lst end?) (take n lst)])
    (format "(~A~A)" (string-join lst ", ")
            (if end? "" ", ..."))))

(define (to-string thing)
  (type-case "string_.toString" (thing)
             [(string) (object-value thing)]
             [(not-provided) "NOT_PROVIDED"]
             [(undefined) "UNDEFINED"]
             [(number) (number->string (object-value thing))]
             [(boolean)
              (if (object-value thing) "true" "false")]
             [(function)
              (match (object-value thing)
                [(struct object/function (argument-name-list _ _))
                 (format "~~~A {...}" (limitied-list 3 argument-name-list))]
                [else "~(...) {...}"])]
             [(array)
              (call-with-output-string
               (lambda (port)
                 (display "[" port)
                 (define items
                   (for/list ([object (in-vector (object-value thing))])
                     (to-string object)))
                 (display (string-join items ", ") port)
                 (display "]" port)))]
             [(package)
              (match-define (struct package (direct-mapping linked-package-list))
                (object-package thing))
              (call-with-output-string
               (lambda (port)
                 (display "@" port)
                 (display (~>> direct-mapping hash-keys (limitied-list 3)) port)
                 (display " {" port)
                 (display "..." port)
                 (define used (length linked-package-list))
                 (unless (zero? used)
                   (display (format "use(~A)" used) port))
                 (display "}" port)))]
             [else "@() {...}"]))

(define-library-package string_
  (def (toString thing)
    (make-object/STRING (to-string thing))))
