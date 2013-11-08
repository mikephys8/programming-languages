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

; 4. Write a function stream-for-n-steps that takes a stream s and a number n. It returns a list holding
; the first n values produced by s in order. Assume n is non-negative. Sample solution: 5 lines. Note:
; You can test your streams with this function instead of the graphics code.
(define (stream-for-n-steps s n)
  (if (equal? n 0)
      null
      (let ([next (s)])
        (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

; 5. Write a stream funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...)
; except numbers divisble by 5 are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream
; is a thunk that when called produces a pair. Here the car of the pair will be a number and the cdr will
; be another stream.
(define funny-number-stream
  (letrec ([f (lambda (x) 
                (cons (if (equal? (remainder x 5) 0) (- 0 x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; 6. Write a stream dan-then-dog, where the elements of the stream alternate between the strings "dan.jpg"
; and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk that
; when called produces a pair of "dan.jpg" and a thunk that when called produces a pair of "dog.jpg"
; and a thunk that when called... etc. Sample solution: 4 lines.
(define dan-then-dog
  (lambda () (cons "dan.jpg"
                   (lambda () (cons "dog.jpg"
                                    dan-then-dog)))))

; 7. Write a function stream-add-zero that takes a stream s and returns another stream. If s would
; produce v for its ith element, then (stream-add-zero s) would produce the pair (0 . v) for its
; ith element. Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion.
; Note: One of the provided tests in the file using graphics uses (stream-add-zero dan-then-dog)
; with place-repeatedly.
(define (stream-add-zero s)
  (let ([next (s)])
    (lambda () (cons (cons 0 (car next))
                     (stream-add-zero (cdr next))))))

; 8. Write a function cycle-lists that takes two lists xs and ys and returns a stream. The lists may or
; may not be the same length, but assume they are both non-empty. The elements produced by the
; stream are pairs where the first part is from xs and the second part is from ys. The stream cycles
; forever through the lists. For example, if xs is '(1 2 3) and ys is '("a" "b"), then the stream
; would produce, (1 . "a"), (2 . "b"), (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"),(2 . "b"), etc.
; Sample solution is 6 lines and is more complicated than the previous stream problems. Hints: Use one
; of the functions you wrote earlier. Use a recursive helper function that takes a number n and calls
; itself with (+ n 1) inside a thunk.
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; 9. Write a function vector-assoc that takes a value v and a vector vec. It should behave like Racket's
; assoc library function except
; (1) it processes a vector (Racket's name for an array) instead of a list,
; (2) it allows vector elements not to be pairs in which case it skips them, and
; (3) it always takes exactly two arguments. Process the vector elements in order starting from 0. You must use library functions
; vector-length, vector-ref, and equal?. Return #f if no vector element is a pair with a car field
; equal to v, else return the first pair with an equal car field. Sample solution is 9 lines, using one local
; recursive helper function.
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(equal? n (vector-length vec)) #f]
                      [(pair? (vector-ref vec n)) (if (equal? (car (vector-ref vec n)) v) (vector-ref vec n) (f (+ n 1)))]
                      [#t (f (+ n 1))]))]) (f 0)))

; 10. Write a function cached-assoc that takes a list xs and a number n and returns a function that takes
; one argument v and returns the same thing that (assoc v xs) would return. However, you should
; use an n-element cache of recent results to possibly make this function faster than just calling assoc
;(if xs is long and a few elements are returned often). The cache must be a Racket vector of length n
; that is created by the call to cached-assoc (use Racket library function vector or make-vector) and
; used-and-possibly-mutated each time the function returned by cached-assoc is called. Assume n is
; positive.
; Hints:
; *In addition to a variable for holding the vector whose contents you mutate with vector-set!,
; use a second variable to keep track of which cache slot will be replaced next. After modifying the
; cache, increment this variable (with set!) or set it back to 0.
; *To test your cache, it can be useful to add print expressions so you know when you are using the
; cache and when you are not. But remove these print expressions before submitting your code.
; *Sample solution is 15 lines.
(define (cached-assoc xs n) (letrec ([cache (make-vector n #f)]
                                     [cache-index 0])
                              (lambda (v)
                                (letrec ([cached (vector-assoc v cache)])
                                  (if cached
                                      (cdr cached)
                                      (letrec ([newval (assoc v xs)])
                                        (begin (set! cache-index (if (= (+ cache-index 1) n) 0 (+ cache-index 1)))
                                               (vector-set! cache cache-index (cons v newval))
                                               newval)))))))