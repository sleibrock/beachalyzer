#lang racket

(provide cprint)

; Print out a number
; Has to be separated by commas
; For this case we won't be printing floats
; For every call of icp, take a character and reduce n by 1
; When n hits 0, set n back to 2 and add a comma
(define (cprint number)
  (define (icp num out n)
    (if (empty? num)
        (list->string out)
        (icp
         (rest num)
         (if (and (not (empty? (rest num))) (= n 0))
             (cons #\, (cons (first num) out))
             (cons (first num) out))
         (if (= n 0)
             2
             (sub1 n)))))
  (icp (reverse (string->list (number->string number))) '() 2))
