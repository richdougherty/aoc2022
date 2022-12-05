#lang racket/base

(require racket/block)
(require racket/file)
(require racket/function)
(require racket/list)
(require racket/match)
(require racket/stream)
(require racket/string)
(require racket/trace)
(require rackunit)

;;; Input files ;;;

(define example-lines (file->lines "example"))
(define input-lines (file->lines "input"))

;;; Stacks data structure

(define (make-stacks num)
  (for/fold ([acc (hash)])
            ([i (in-range 1 (+ num 1))])
    (hash-set acc i '())))

(check-equal? (make-stacks 0) (hash))
(check-equal? (make-stacks 1) (hash 1 '()))
(check-equal? (make-stacks 2) (hash 1 '() 2 '()))

(define (stacks-ordered-keys stacks)
  (stream->list (in-range 1 (+ (hash-count stacks) 1))))

(define (stacks-ordered-values stacks)
  (map (curry hash-ref stacks) (stacks-ordered-keys stacks)))

;;; Parsing stacks

(define (stacks-add-line stacks line)
  (for/fold ([stacks-acc stacks])
            ([i (hash-keys stacks)])
    (let* ([pos (- (* i 4) 3)] ; stack 1 = pos 1, stack 2 = pos 5, stack 3 = pos 9
           [char (string-ref line pos)])
      (if (equal? char #\space)
          stacks-acc
          (hash-update stacks-acc i (curry cons char))))))

(check-equal? (stacks-add-line (make-stacks 3) "[Z] [M] [P]") (hash 1 '(#\Z) 2 '(#\M) 3 '(#\P)))
(check-equal? (stacks-add-line (make-stacks 3) "[N] [C]    ") (hash 1 '(#\N) 2 '(#\C) 3 '()))

(define (parse-stack-lines stack-lines)
   (let* ([reversed-stack-lines (reverse stack-lines)]
          [number-line (car reversed-stack-lines)]
          [number-of-stacks (/ (+ (string-length number-line) 1) 4)]
          [crate-lines (cdr reversed-stack-lines)])
     (for/fold ([stacks-acc (make-stacks number-of-stacks)])
               ([crate-line crate-lines])
       (stacks-add-line stacks-acc crate-line))))

(check-equal? (parse-stack-lines (list "    [D]    "
                                       "[N] [C]    "
                                       "[Z] [M] [P]"
                                       " 1   2   3 "))
              (hash 1 '(#\N #\Z) 2 '(#\D #\C #\M) 3 '(#\P)))

;;; Moves data structure

(struct move (count from to) #:transparent)

;;; Parsing moves

(define (parse-move-line move-line)
  (match (string-split move-line)
    [(list _ amount-str _ from-str _ to-str)
     (apply move
            (map string->number
                 (list amount-str from-str to-str)))]))

(check-equal? (parse-move-line "move 1 from 2 to 1") (move 1 2 1))
(check-equal? (parse-move-line "move 23 from 6 to 1") (move 23 6 1))

(define (parse-move-lines move-lines)
  (map parse-move-line move-lines))

;;; Parsing file

(struct puzzle (stacks moves) #:transparent)


(define (parse-puzzle-lines file-lines)
  (let-values ([(start-lines end-lines) (splitf-at file-lines (λ (line) (> (string-length line) 0)))])
    (let* ([stack-lines start-lines]
           [move-lines (cdr end-lines)]
           [stacks (parse-stack-lines stack-lines)]
           [moves (parse-move-lines move-lines)])
      (puzzle stacks moves))))

(check-equal? (parse-puzzle-lines example-lines)
              (puzzle (hash 1 '(#\N #\Z) 2 '(#\D #\C #\M) 3 '(#\P))
                      (list (move 1 2 1)
                            (move 3 1 3)
                            (move 2 2 1)
                            (move 1 1 2))))

;;; Applying moves

(define (stacks-apply-move stacks move reverse?)
  (let*-values ([(from) (move-from move)]
                [(to) (move-to move)]
                [(count) (move-count move)]
                [(old-from-crates) (hash-ref stacks from)]
                [(old-to-crates) (hash-ref stacks to)]
                [(removed-crates new-from-crates) (split-at old-from-crates count)]
                [(placed-crates) (if reverse? (reverse removed-crates) removed-crates)]
                [(new-to-crates) (append placed-crates old-to-crates)])
               (hash-set* stacks
                          from new-from-crates
                          to new-to-crates)))

(check-equal? (stacks-apply-move (hash 1 '(#\N #\Z) 2 '(#\D #\C #\M) 3 '(#\P))
                                 (move 1 2 1)
                                 #t)
              (hash 1 '(#\D #\N #\Z) 2 '(#\C #\M) 3 '(#\P)))
(check-equal? (stacks-apply-move (hash 1 '(#\D #\N #\Z) 2 '(#\C #\M) 3 '(#\P))
                                 (move 3 1 3)
                                 #t)
              (hash 1 '() 2 '(#\C #\M) 3 '(#\Z #\N #\D #\P)))

(check-equal? (stacks-apply-move (hash 1 '() 2 '(#\C #\M) 3 '(#\Z #\N #\D #\P))
                                 (move 2 2 1)
                                 #t)
              (hash 1 '(#\M #\C) 2 '() 3 '(#\Z #\N #\D #\P)))

(define (stacks-apply-all-moves stacks moves reverse?)
  (for/fold ([acc-stacks stacks])
            ([move moves])
    (stacks-apply-move acc-stacks move reverse?)))

(check-equal? (let ([puzzle (parse-puzzle-lines example-lines)])
                (stacks-apply-all-moves (puzzle-stacks puzzle) (puzzle-moves puzzle) #t))
              (hash 1 '(#\C) 2 '(#\M) 3 '(#\Z #\N #\D #\P)))

(define (puzzle-answer lines reverse?)
  (let* ([puzzle (parse-puzzle-lines lines)]
         [final-stacks (stacks-apply-all-moves (puzzle-stacks puzzle) (puzzle-moves puzzle) reverse?)])
    (apply string (map car (stacks-ordered-values final-stacks)))))



;;; Part 1 answers

(define (part1-answer lines)
  (puzzle-answer lines #t))

(check-equal? (part1-answer example-lines) "CMZ")
(displayln (part1-answer input-lines))

;;; Part 2 answers


(define (part2-answer lines)
  (puzzle-answer lines #f))

(check-equal? (part2-answer example-lines) "MCD")
(displayln (part2-answer input-lines))
