#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define (sequence low high stride)
  (if (> low high) '() (cons low (sequence (+ low stride) high stride))))
