#lang racket

(require "data/Tables.rkt")
(require racket/trace)

; String -> List
(define (interpret-file file-name)
  (let*
    ((infile (open-input-file file-name))
     (data (accum-file '() infile)))
    (close-input-port infile)
    data))

; Recursive file accumulator
(define (accum-file acc prt)
  (let
    ((val (read-line prt)))
    (if (eof-object? val)
      acc
      (accum-file
        (cons val acc) prt))))

(define (parse-string input-str)
  (let
    ((new-data (string-split input-str ":")))
    (list
      (car new-data)
      (string->number (string-trim (car (cdr new-data)))))))


(define (find-lowest in-data buildings lowest-pair)
  (if (empty? in-data)
    lowest-pair
    (find-lowest
      (cdr in-data)
      buildings
      (let
        ((tc (get-cost (first in-data) buildings)))
        (if (> (second lowest-pair) (second tc))
          tc
          lowest-pair)))))

(define (get-cost in-pair buildings)
  (if (string=? (first in-pair)
                (building-name 
                  (first buildings)))
    (list (first in-pair)
          (if (=
                (length 
                  (building-table (first buildings)))
                (second in-pair))
          +inf.0
          (total-cost
            (list-ref
              (building-table (first buildings))
              (second in-pair)))))
    (get-cost in-pair (cdr buildings))))

(define (get-hq-pair in-data)
  (if (empty? in-data)
    '("Nothing" 0)
    (if (string=? "headquarters" (first (first in-data)))
      (first in-data)
      (get-hq-pair (cdr in-data)))))

(command-line
  #:program "Beachalyzer"
  #:args (infile-name)
  (let*
    ((data (map parse-string (interpret-file infile-name)))
     (lowest (find-lowest data all-buildings (list "nil" +inf.0)))
     (hq (get-hq-pair data))
     (diff (round (* 100 (exact->inexact 
             (/ (second lowest) 
                (second (get-cost hq all-buildings)))))))
     )
    (printf "Your HQ level is: ~a\n" (second hq))
    (printf "Your lowest cost building is: ~a\n" (first lowest))
    (printf "Cost of ~a is: ~a\n" (first lowest) (second lowest))
    (printf "Cost of next HQ is: ~a\n" (second (get-cost hq all-buildings)))
    (printf "Differential between lowest and HQ is: ~a%\n" diff)))
