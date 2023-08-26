#lang racket

(require threading)
(require basic-cc/tokenization)

(define (parse-tree->oneside-token side-function parse-tree)
  (if (token? parse-tree)
      parse-tree
      (let ([children (rest parse-tree)])
        (parse-tree->oneside-token side-function (side-function children)))))

(define (parse-tree->oneside-location side-function parse-tree)
  (~> (parse-tree->oneside-token side-function parse-tree)
      token-location))

(define (parse-tree->location-string parse-tree)
  (define left-most (parse-tree->oneside-location first parse-tree))
  (define right-most (parse-tree->oneside-location last parse-tree))
  (if (equal? left-most right-most)
      (format "~A: L~AC~A" (location-file left-most)
              (add1 (location-line left-most)) (add1 (location-column left-most)))
      (format "~A: L~AC~A...L~AC~A" (location-file left-most)
              (add1 (location-line left-most)) (add1 (location-column left-most))
              (add1 (location-line right-most)) (add1 (location-column right-most)))))

(provide parse-tree->location-string)

(define (sub-list lst start-idx end-idx)
  (take (drop lst start-idx) (add1 (- end-idx start-idx))))

(define (map+ mapper/first mapper/middle mapper/last lst)
  (match lst
    [(list) (list)]
    [(list elt) (list (mapper/last (mapper/first elt)))]
    [(list hd tl) (list (mapper/first hd) (mapper/last tl))]
    [(list hd mid ... tl) (append (list (mapper/first hd))
                                  (map mapper/middle mid)
                                  (list (mapper/last tl)))]))

(define (underline lines start-line-number end-line-number start-column end-column)
  (define underlined#1
    (map (lambda (line) (list line (make-string (string-length line) #\~))) lines))
  (define underlined#2
    (map+ (match-lambda
            [(list line underline)
             (list line
                   (~a (substring underline start-column)
                       #:width (string-length underline)
                       #:align 'right))])
          identity
          (match-lambda
            [(list line underline) (list line (substring underline 0 end-column))])
          underlined#1))
  (define line-number-width
    (add1 (~> end-line-number add1 ~a string-length)))
  (define (line-number->prefix-string n)
    (~a n #:width line-number-width #:align 'right))
  (define line-numbered
    (~> (for/list ([line+underline (in-list underlined#2)]
                   [line-number (in-naturals (add1 start-line-number))])
          (list (format "~A| ~A" (line-number->prefix-string line-number) (first line+underline))
                (format "~A| ~A" (make-string line-number-width #\space) (second line+underline))))
        flatten))
  (string-join line-numbered "\n"))

(define (locate-source parse-tree)
  (define start-token (parse-tree->oneside-token first parse-tree))
  (define end-token (parse-tree->oneside-token last parse-tree))
  (define start-location (token-location start-token))
  (define end-location (token-location end-token))
  (match-define (struct location (path start-line start-column)) start-location)
  (match-define (struct location (_ end-line end-column)) end-location)
  (define lines (file->lines path #:mode 'text))
  (define source-lines
    (sub-list lines start-line end-line))
  (underline source-lines start-line end-line start-column (+ (~> end-token token-text string-length) end-column)))

(provide locate-source)
