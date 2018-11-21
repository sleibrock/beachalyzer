#lang racket

(require "data/Tables.rkt")

(define (write-blank-file name list-names)
  (let
    ((outfile (open-output-file name #:exists 'truncate)))
    (for-each
      (lambda (n)
        (displayln
          (string-append n ": 0")
          outfile))
      list-names)
    (close-output-port outfile)))


; file generating entry point
(command-line
  #:program "File Generator"
  #:args (outfile-name)
  (displayln
    (string-append "Creating a template in '"
                   outfile-name
                   "' ... "))
  (write-blank-file
    outfile-name
    (map building-name all-buildings))
  (displayln "Finished!"))

; end
