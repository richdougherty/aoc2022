#lang racket/base

(require racket/block)
(require racket/file)
(require racket/list)
(require racket/match)
(require racket/string)
(require rackunit)

(define example-lines (file->lines "example"))
(define input-lines (file->lines "input"))



;;; Parsing ;;;

(struct assignment (start end) #:transparent)

(define (string->assignment str)
  (match (string-split str "-")
    [(list start-str end-str) (assignment
                               (string->number start-str)
                               (string->number end-str))]))

(check-equal? (string->assignment "2-4") (assignment 2 4))
(check-equal? (string->assignment "6-8") (assignment 6 8))
(check-equal? (string->assignment "4-33") (assignment 4 33))
(check-equal? (string->assignment "11-73") (assignment 11 73))

(define (string->assignment-pair str)
  (match (string-split str ",")
    [(list a-str b-str) (cons (string->assignment a-str) (string->assignment b-str))]))

(check-equal? (string->assignment-pair "2-8,3-7")     (cons (assignment 2 8)   (assignment 3 7)))
(check-equal? (string->assignment-pair "7-98,8-97")   (cons (assignment 7 98)  (assignment 8 97)))
(check-equal? (string->assignment-pair "7-42,43-55")  (cons (assignment 7 42)  (assignment 43 55)))
(check-equal? (string->assignment-pair "69-80,5-44")  (cons (assignment 69 80) (assignment 5 44)))
(check-equal? (string->assignment-pair "10-30,30-43") (cons (assignment 10 30) (assignment 30 43)))

;;; Part 1 logic ;;;

(define (fully-contains a b)
  (and (<= (assignment-start a) (assignment-start b)) (>= (assignment-end a) (assignment-end b))))

(check-equal? (fully-contains (assignment 2 4) (assignment 6 8)) #f)
(check-equal? (fully-contains (assignment 5 7) (assignment 7 9)) #f)
(check-equal? (fully-contains (assignment 2 8) (assignment 3 7)) #t)
(check-equal? (fully-contains (assignment 6 6) (assignment 4 6)) #f)
(check-equal? (fully-contains (assignment 4 6) (assignment 6 6)) #t)

(define (either-fully-contains assignment-pair)
  (let* ([a (car assignment-pair)]
         [b (cdr assignment-pair)])
    (or
     (fully-contains a b)
     (fully-contains b a))))

(check-equal? (either-fully-contains (cons (assignment 2 4) (assignment 6 8))) #f)
(check-equal? (either-fully-contains (cons (assignment 5 7) (assignment 7 9))) #f)
(check-equal? (either-fully-contains (cons (assignment 2 8) (assignment 3 7))) #t)
(check-equal? (either-fully-contains (cons (assignment 6 6) (assignment 4 6))) #t)
(check-equal? (either-fully-contains (cons (assignment 4 6) (assignment 6 6))) #t)


(define (list-fully-contained lines)
  (for/list ([assignment-pair (map string->assignment-pair lines)]
             #:when (either-fully-contains assignment-pair))
    assignment-pair))

;;; Part 1 answers ;;;

(check-equal? (length (list-fully-contained example-lines)) 2)
(displayln (length (list-fully-contained input-lines)))


;;; Part 2 logic ;;;;

(define (point-contained-in x assmt)
  (and (>= (assignment-start assmt) x) (<= x (assignment-end assmt))))

(check-equal? (point-contained-in 1 (assignment 1 2)) #t)
(check-equal? (point-contained-in 1 (assignment 2 4)) #f)
(check-equal? (point-contained-in 2 (assignment 1 2)) #t)
(check-equal? (point-contained-in 3 (assignment 1 2)) #f)

(define (overlaps a b)
  (and (<= (assignment-start a) (assignment-start b)) (>= (assignment-end a) (assignment-end b))))