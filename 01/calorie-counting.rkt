#lang racket/base

(require data/maybe)
(require racket/block)
(require racket/file)
(require racket/list)
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

(define (sum-list lst)
  (foldl + 0 lst))

(check-equal? (sum-list '()) 0)
(check-equal? (sum-list '(1)) 1)
(check-equal? (sum-list '(1 2)) 3)

;;;

(define (sum-sublists list-of-sublists)
  (map sum-list list-of-sublists))

(check-equal? (sum-sublists '()) '())
(check-equal? (sum-sublists '((1))) '(1))
(check-equal? (sum-sublists '((1 2))) '(3))
(check-equal? (sum-sublists '((1 2) (3 4) () (5 6 7))) '(3 7 0 18))

(define example-file-summed-sublists (sum-sublists example-file-numbers))
(check-equal? example-file-summed-sublists '(6000 4000 11000 24000 10000))

;;;

(define (find-max lst)
  (foldl (λ (x result)
           (just (maybe x (λ (result-val) (max x result-val)) result)))
         nothing
         lst))

(check-equal? (find-max '(3 7 0 18)) (just 18))

(define example-file-max (find-max example-file-summed-sublists))
(check-equal? example-file-max (just 24000))

;;;

(define (summed-sublists-from-file file-name)
  (let* ([file-lines (file->lines file-name)]
         [split-lines (split-lines-on-empty file-lines)]
         [numbers (sublist-strings->numbers split-lines)]
         [summed-sublists (sum-sublists numbers)])
    summed-sublists))


(check-equal? (summed-sublists-from-file "example") '(6000 4000 11000 24000 10000))

;;;

(define (max-from-file file-name)
  (let* ([summed-sublists (summed-sublists-from-file file-name)]
         [maybe-max (find-max summed-sublists)]
         [max (from-just! maybe-max)])
    max))

(check-equal? (max-from-file "example") 24000)

;;; Answer to Part 1

(displayln (max-from-file "input"))

;;; Part 2 ;;;

(define (take-sorted count lst less-than?)
  (take (sort lst less-than?) count))

(check-equal? (take-sorted 0 '() <) '())
(check-equal? (take-sorted 0 '(1 2 3) <) '())
(check-equal? (take-sorted 1 '(1 2 3) <) '(1))
(check-equal? (take-sorted 2 '(1 2 3) <) '(1 2))
(check-equal? (take-sorted 3 '(1 2 3) <) '(1 2 3))
(check-equal? (take-sorted 3 '(1 2 3 4 5) <) '(1 2 3))
(check-equal? (take-sorted 3 '(1 2 3 4 5) >) '(5 4 3))


(define example-file-top-3 (take-sorted 3 example-file-summed-sublists >))
(check-equal? example-file-top-3  '(24000 11000 10000))

;

(define (sum-top-3 lst)
  (sum-list (take-sorted 3 lst >)))

(define example-file-top-3-sum (sum-top-3 example-file-summed-sublists))
(check-equal? example-file-top-3-sum  45000)

;;;

(define (sum-top-3-from-file file-name)
  (let* ([summed-sublists (summed-sublists-from-file file-name)]
         [sum (sum-top-3 summed-sublists)])
    sum))

(check-equal? (sum-top-3-from-file "example") 45000)

;;; Answer to Part 2

(displayln (sum-top-3-from-file "input"))