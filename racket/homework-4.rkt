#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; 1. Write a function sequence that takes 3 arguments low, high, and stride, all assumed to be numbers.
; Further assume stride is positive. sequence produces a list of numbers from low to high (including
; low and possibly high) separated by stride and in sorted order. Sample solution: 4 lines.
(define (sequence low high stride)
  (if (> low high) '() (cons low (sequence (+ low stride) high stride))))

; 2. Write a function string-append-map that takes a list of strings xs and a string suffix and returns a
; list of strings. Each element of the output should be the corresponding element of the input appended
; with suffix (with no extra space between the element and suffix). You must use Racket-library
; functions map and string-append. Sample solution: 2 lines.
(define (string-append-map xs suffix) (map (lambda (x) (string-append x suffix)) xs))

; 3. Write a function list-nth-mod that takes a list xs and a number n. If the number is negative,
; terminate the computation with (error "list-nth-mod: negative number"). Else if the list is
; empty, terminate the computation with (error "list-nth-mod: empty list"). Else return the ith
; element of the list where we count from zero and i is the remainder produced when dividing n by the
; list's length. Library functions length, remainder, car, and list-tail are all useful { see the Racket
; documentation. Sample solution is 6 lines.
(define (list-nth-mod xs n) 
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([rem (remainder n (length xs))]) (car (list-tail xs rem)))]))