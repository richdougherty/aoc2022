#lang racket/base

(require data/maybe)
(require racket/block)
(require racket/file)
(require racket/list)
(require racket/match)
(require racket/string)
(require rackunit)

; First, read the example file as simple strings

(define example-file-lines (file->lines "example"))

(check-equal? example-file-lines '("A Y" "B X" "C Z"))


;;;;; PART 1 ;;;;;

;;; define constant mappings

(define their-shapes
  #hash(
   ("A" . rock)
   ("B" . paper)
   ("C" . scissors)))
(define my-shapes
  #hash(
   ("X" . rock)
   ("Y" . paper)
   ("Z" . scissors)))
(define my-shape-scores
  #hash(
   (rock . 1)
   (paper . 2)
   (scissors . 3)))
(define outcome-scores
  #hash(
   (lose . 0)
   (draw . 3)
   (win . 6)))

;;; round-string->shapes - parse a round string into a pair of shapes

(define (round-string->shapes round-string)
  (let* ([parts (string-split round-string)]
         [their-shape (hash-ref their-shapes (first parts))]
         [my-shape (hash-ref my-shapes (second parts))])
   (cons their-shape my-shape)))

; check examples

(check-equal? (round-string->shapes "A Y") '(rock . paper))
(check-equal? (round-string->shapes "B X") '(paper . rock))
(check-equal? (round-string->shapes "C Z") '(scissors . scissors))

;;; round-outcome - find the winner for a round

(define (round-outcome round-shapes)
  (match round-shapes
    [(cons 'rock 'rock) 'draw]
    [(cons 'rock 'paper) 'win]
    [(cons 'rock 'scissors) 'lose]
    [(cons 'paper 'rock) 'lose]
    [(cons 'paper 'paper) 'draw]
    [(cons 'paper 'scissors) 'win]
    [(cons 'scissors 'rock) 'win]
    [(cons 'scissors 'paper) 'lose]
    [(cons 'scissors 'scissors) 'draw]))

; check examples

(check-equal? (round-outcome '(rock . paper)) 'win)
(check-equal? (round-outcome '(paper . rock)) 'lose)
(check-equal? (round-outcome '(scissors . scissors)) 'draw)

;;; round-score - find the score for a round


(define (round-score round-shapes)
  (+ (hash-ref outcome-scores (round-outcome round-shapes))
     (hash-ref my-shape-scores (cdr round-shapes))))

; check examples

(check-equal? (round-score '(rock . paper)) 8)
(check-equal? (round-score '(paper . rock)) 1)
(check-equal? (round-score '(scissors . scissors)) 6)

;;; lines->total-score

(define (lines->total-score lines)
  (apply + (map (λ (line)
                  (let* ([shapes (round-string->shapes line)]
                         [score (round-score shapes)])
                    score))
                lines)))

; check example total

(check-equal? (lines->total-score (file->lines "example")) 15)

; Answer for Part 1

(displayln (lines->total-score (file->lines "input")))

;;;;; PART 2 ;;;;;

(define desired-outcomes
  #hash(
   ("X" . lose)
   ("Y" . draw)
   ("Z" . win)))

;;;

(define (round-string->round-plan2 round-string)
  (let* ([parts (string-split round-string)]
         [their-shape (hash-ref their-shapes (first parts))]
         [desired-outcome (hash-ref desired-outcomes (second parts))])
   (cons their-shape desired-outcome)))

; Check examples

(check-equal? (round-string->round-plan2 "A Y") '(rock . draw))
(check-equal? (round-string->round-plan2 "B X") '(paper . lose))
(check-equal? (round-string->round-plan2 "C Z") '(scissors . win))

;;;

(define (my-shape-for-round-plan2 round-plan2)
  (match round-plan2
    [(cons 'rock 'lose) 'scissors]
    [(cons 'rock 'draw) 'rock]
    [(cons 'rock 'win) 'paper]
    [(cons 'paper 'lose) 'rock]
    [(cons 'paper 'draw) 'paper]
    [(cons 'paper 'win) 'scissors]
    [(cons 'scissors 'lose) 'paper]
    [(cons 'scissors 'draw) 'scissors]
    [(cons 'scissors 'win) 'rock]))

; check examples

(check-equal? (my-shape-for-round-plan2 '(rock . draw)) 'rock)
(check-equal? (my-shape-for-round-plan2 '(paper . lose)) 'rock)
(check-equal? (my-shape-for-round-plan2 '(scissors . win)) 'rock)

(define (lines2->total-score lines)
  (apply + (map (λ (line)
                  (let* ([plan2 (round-string->round-plan2 line)]
                         [my-shape (my-shape-for-round-plan2 plan2)]
                         [their-shape (car plan2)]
                         [plan1 (cons their-shape my-shape)]
                         [score (round-score plan1)])
                    score))
                lines)))

;
(check-equal? (lines2->total-score (file->lines "example")) 12)

(displayln (lines2->total-score (file->lines "input")))
