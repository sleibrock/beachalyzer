#lang racket

(require "data/Tables.rkt")
(require "Functions.rkt")

;; All code that defines operations upon buildings goes here
; Search for buildings that cost less than the previous
; Initially buildings have to cost less than infinity
; If nothing is actually left, then return infinity
(define (find-lowest in-data buildings lowest-pair)
  (if (empty? in-data)
    lowest-pair
    (find-lowest
      (rest in-data)
      buildings
      (let
        ((tc (get-cost (first in-data) buildings)))
        (if (> (second lowest-pair) (second tc))
          tc
          lowest-pair)))))

; Get the cost of an upgrade
; in-pair is a '(name level)
; matches in-pair to find it's next level upgrade
; Example: (get-cost '("headquarters" 18) buildings) => 6320000
(define (get-cost in-pair buildings)
  (list 
    (first in-pair)
    (total-cost
    (list-ref
      (get-levels (first in-pair) buildings)
      (sub1 (second in-pair))))))

; Search for the HQ level
; Error if HQ was not found
(define (get-hq-pair in-data)
  (if (empty? in-data)
    (error "No HQ found in input file")
    (if (string=? "headquarters" (first (first in-data)))
      (first in-data)
      (get-hq-pair (cdr in-data)))))

; Get the total average cost of all possible upgrades
; Base case when (empty? in-data)
; acc and n are mean calculated (mean = acc/n)
; if cost of building is +inf.0, add 0 instead
(define (mean-upgrade-cost in-data buildings acc n)
  (if (empty? in-data)
      (round (inexact->exact (round (/ acc n))))
      (let
          ((cost (second (get-cost (first in-data) buildings))))
        (mean-upgrade-cost
         (rest in-data)
         buildings
         (+ acc (if (= cost +inf.0) 0 cost))
         (+ n   (if (= cost +inf.0) 0 1))))))

(command-line
  #:program "Beachalyzer"
  #:args (infile-name)
  (define data (map parse-string (interpret-file infile-name)))
  (define lowest (find-lowest data all-buildings (list "nil" +inf.0)))
  (define hq (get-hq-pair data))
  (define diff (round (* 100 (exact->inexact
                               (/ (second lowest)
                                  (second (get-cost hq all-buildings)))))))
  (define avg (mean-upgrade-cost data all-buildings 0 0))
  (printf "Your HQ level is: ~a\n" (second hq))
  (printf "Your lowest cost building is: ~a\n" (first lowest))
  (printf "Cost of ~a is: ~a\n" (first lowest) (cprint (second lowest)))
  (printf "Cost of next HQ is: ~a\n" (cprint (second (get-cost hq all-buildings))))
  (printf "Ratio between lowest and HQ is: ~a%\n" diff)
  (printf "Average cost of upgrades is: ~a\n" (cprint avg)))
