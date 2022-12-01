#lang racket/base

(require data/maybe)
(require racket/file)
(require rackunit)

;;;

(define example-file-lines (file->lines "example"))
(check-equal? example-file-lines '("1000" "2000" "3000" "" "4000" "" "5000" "6000" "" "7000" "8000" "9000" "" "10000"))

;;;

; Split a list into a list of sublists, splitting at a defined predicate
(define (split-list split-pred? lst)

  (define (loop loop-list finished-lists curr)

    (define (finished-with-curr-added)
      (if curr
          (cons (reverse curr) finished-lists)
          finished-lists))
    
    (if (null? loop-list)
        ; No more elements to process, return result
        (reverse (finished-with-curr-added))
        ; Iterate
        (let* ([loop-element (car loop-list)]
               [loop-tail (cdr loop-list)])
          (if (split-pred? loop-element)
              ; split - existing curr-list is added to finished-lists
              (loop loop-tail
                    (finished-with-curr-added)
                    #f)
              ; continue - add to existing curr-list
              (let ([curr-list (if curr curr '())])
                (loop loop-tail
                      finished-lists
                      (cons loop-element curr-list)))))))

  (loop lst '() #f))

(check-equal? (split-list even? '()) '())
(check-equal? (split-list even? '(1)) '((1)))
(check-equal? (split-list even? '(1 2 3)) '((1) (3)))
(check-equal? (split-list even? '(1 2 2 3)) '((1) (3)))
(check-equal? (split-list even? '(0 1 0)) '((1)))
(check-equal? (split-list even? '(1 2 3 4 5 6)) '((1) (3) (5)))

;;;

(define (split-lines-on-empty lines)
  (split-list (λ (str) (eq? (string-length str) 0)) lines))

(check-equal? (split-lines-on-empty '("1" "" "3")) '(("1") ("3")))

(define example-file-split-lines (split-lines-on-empty example-file-lines))
(check-equal? example-file-split-lines '(("1000" "2000" "3000") ("4000") ("5000" "6000") ("7000" "8000" "9000") ("10000")))


;;;

(define (sublist-strings->numbers lst)
  (map (λ (sublist)
         (map string->number sublist))
       lst))

(check-equal? (sublist-strings->numbers '()) '())
(check-equal? (sublist-strings->numbers '(("0"))) '((0)))
(check-equal? (sublist-strings->numbers '(("1" "2") ("3"))) '((1 2) (3)))

(define example-file-numbers (sublist-strings->numbers example-file-split-lines))
(check-equal? example-file-numbers '((1000 2000 3000) (4000) (5000 6000) (7000 8000 9000) (10000)))

;;;

(define (sum-sublists list-of-sublists)
  (map (λ (sublist)
         (foldl + 0 sublist))
       list-of-sublists))

(check-equal? (sum-sublists '()) '())
(check-equal? (sum-sublists '((1))) '(1))
(check-equal? (sum-sublists '((1 2))) '(3))
(check-equal? (sum-sublists '((1 2) (3 4) () (5 6 7))) '(3 7 0 18))

(define example-file-summed (sum-sublists example-file-numbers))
(check-equal? example-file-summed '(6000 4000 11000 24000 10000))

;;;

(define (find-max lst)
  (foldl (λ (x result)
           (just (maybe x (λ (result-val) (max x result-val)) result)))
         nothing
         lst))

(check-equal? (find-max '(3 7 0 18)) (just 18))

(define example-file-max (find-max example-file-summed))
(check-equal? example-file-max (just 24000))

;;;

(define (max-from-file file-name)
  (let* ([file-lines (file->lines file-name)]
         [split-lines (split-lines-on-empty file-lines)]
         [numbers (sublist-strings->numbers split-lines)]
         [summed (sum-sublists numbers)]
         [maybe-max (find-max summed)])
    (from-just! maybe-max)))

(check-equal? (max-from-file "example") 24000)

;;;

(displayln (max-from-file "input"))