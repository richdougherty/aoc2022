#lang racket/base

(require racket/file)
(require racket/list)
(require racket/string)
(require racket/list)
(require rackunit)

(define (common-items item-lists)
  (let* ([first-item-list (car item-lists)]
         [other-item-lists (cdr item-lists)])    
    (for/fold ([acc '()])
              ([c first-item-list]
               #:when (not (member c acc))
               #:when (andmap (λ (other-list) (member c other-list)) other-item-lists))
      (cons c acc))))


(check-equal? (common-items '(() () ())) '())
(check-equal? (common-items '((1) () ())) '())
(check-equal? (common-items '((1) (1) (1))) '(1))

(define (rucksack-compartments rucksack)
  (let*-values ([(chars) (string->list rucksack)]
                [(size) (length chars)]
                [(chars1 chars2) (split-at chars (/ size 2))])
    (list chars1 chars2)))   


(check-equal? (rucksack-compartments "aa") '((#\a) (#\a)))
(check-equal? (rucksack-compartments "abcdef") '((#\a #\b #\c) (#\d #\e #\f)))

(check-equal? (common-items (rucksack-compartments "")) '())
(check-equal? (common-items (rucksack-compartments "aa")) '(#\a))
(check-equal? (common-items (rucksack-compartments "vJrwpWtwJgWrhcsFMMfFFhFp")) '(#\p))
(check-equal? (common-items (rucksack-compartments "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")) '(#\L))
(check-equal? (common-items (rucksack-compartments "PmmdzqPrVvPwwTWBwg")) '(#\P))
  

;(define (common-items rucksack)
;  (let*-values ([(chars) (string->list rucksack)]
;                [(size) (length chars)]
;                [(chars1 chars2) (split-at chars (/ size 2))])
;    (for*/fold ([acc '()])
;               ([c chars1]
;                #:when (not (member c acc))
;                #:when (member c chars2))
;      (cons c acc))))


(define (item-priority item)
  (let ([asc (char->integer item)])
    (if (<= asc 90)
        (- asc 38)    ; Uppercase item types A (ASCII 38) through Z have priorities 27 through 52.
        (- asc 96)))) ; Lowercase item types a (ASCII 97) through z have priorities 1 through 26

(check-equal? (item-priority #\a) 1)
(check-equal? (item-priority #\z) 26)
(check-equal? (item-priority #\A) 27)
(check-equal? (item-priority #\Z) 52)

(define (sum-priorities rucksacks)
  (let ([all-priorities (for*/list ([rucksack rucksacks]
                                    [common-item (common-items (rucksack-compartments rucksack))])
                          (item-priority common-item))])
    (apply + all-priorities)))


(check-equal? (sum-priorities '("aa" "ZZ")) 53)

(check-equal? (sum-priorities (file->lines "example")) 157)
(displayln (sum-priorities (file->lines "input")))

(define (group-by-size lst size)
  (if (null? lst) '()
      (let-values ([(group rest) (split-at lst size)])
        (cons group (group-by-size rest size)))))

(check-equal? (group-by-size '() 3) '())
(check-equal? (group-by-size '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))

(check-equal? (common-items (map string->list '("vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg"))) '(#\r))

(define (sum-badge-priorities rucksacks)
  (apply +
         (for/list ([group (group-by-size rucksacks 3)])
           (let* ([item-lists (map string->list group)]
                  [common (common-items item-lists)]
                  [common-first (car common)] ; assume only 1 (should check probably)
                  [priority (item-priority common-first)])
             priority))))
                  
(check-equal? (sum-badge-priorities (file->lines "example")) 70)
(displayln (sum-badge-priorities (file->lines "input")))