;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1. Warm-Up:
; (a) Write a Racket function racketlist->mupllist that takes a Racket list (presumably of mupl
; values but that will not affect your solution) and produces an analogous mupl list with the same
; elements in the same order.
(define (racketlist->mupllist r-list)
  (cond [(null? r-list) (aunit)]
        [#t (apair (car r-list) (racketlist->mupllist (cdr r-list)))]))

;(b) Write a Racket function mupllist->racketlist that takes a mupl list (presumably of mupl
; values but that will not aect your solution) and produces an analogous Racket list (of mupl
; values) with the same elements in the same order.
(define (mupllist->racketlist m-list)
  (cond [(aunit? m-list) null]
        [#t (cons (apair-e1 m-list) (mupllist->racketlist (apair-e2 m-list)))]))


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str); (int 1))
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (begin (print (var-string e)) (print " ") (print (envlookup env (var-string e))) (print (newline)) (envlookup env (var-string e)))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2)) (eval-under-env (ifgreater-e3 e) env) (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(int? e) e]
        [(mlet? e)
         (letrec ([v (eval-under-env (mlet-e e) env)]
                  [newenv (cons (cons (mlet-var e) v) env)]) ; Add the var to the new environment
           (eval-under-env (mlet-body e) newenv))]
        [(call? e)
         (letrec ([arg (eval-under-env (call-actual e) env)]
               [clos (eval-under-env (call-funexp e) env)]
               [funbody (fun-body (closure-fun clos))]
               [inval (fun-formal (closure-fun clos))])
           (if (closure? clos)
               (eval-under-env (fun-body (closure-fun clos)) (cons (cons inval arg) (closure-env clos)))
               (error "MUPL can't call a function that isn't a closure")))]
        [(fun? e)
         (letrec ([funname (fun-nameopt e)]
                  [newenv (if funname (cons (cons funname clos) env) env)]
                  [clos (closure newenv (fun-body e))]
                  )
                  clos)]
        [(closure? e)
         (closure (append (closure-env e) env) (closure-fun e))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v) (apair-e1 v) (begin (print v) (error "Can only call fst on a pair"))))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v) (apair-e2 v) (error "Can only call snd on a pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
;(a) Write a Racket function ifaunit that takes three mupl expressions e1, e2, and e3. It returns a
; mupl expression that when run evaluates e1 and if the result is mupl's aunit then it evaluates e2
; and that is the overall result, else it evaluates e3 and that is the overall result.
; Sample solution: 1 line.
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

; (b) Write a Racket function mlet* that takes a Racket list of Racket pairs
; '((s1 . e1) ... (si . ei) ... (sn . en)) and a final mupl expression en+1.
; In each pair, assume si is a Racket string and ei is a mupl expression.
; mlet* returns a mupl expression whose value is en+1 evaluated in an
; environment where each si is a variable bound to the result of evaluating the corresponding ei
; for 1 <= i <= n. The bindings are done sequentially, so that each ei is evaluated in an
; environment where s1 through si-1 have been previously bound to the values e1 through ei-1.
(define (mlet* lstlst e2) 
  (if (null? lstlst)
      e2
      (let ([next-binding (car lstlst)]
            [remaining-bindings (cdr lstlst)])
      (mlet (car next-binding) (cdr next-binding) (mlet* remaining-bindings e2)))))

; (c) Write a Racket function ifeq that takes four mupl expressions e1, e2, e3, and e4 and returns
; a mupl expression that acts like ifgreater except e3 is evaluated if and only if e1 and e2 are
; equal integers. Assume none of the arguments to ifeq use the mupl variables _x or _y. Use this
; assumption so that when an expression returned from ifeq is evaluated, e1 and e2 are evaluated
; exactly once each.
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4
; (a) Bind to the Racket variable mupl-map a mupl function that acts like map (as we used extensively
; in ML). Your function should be curried: it should take a mupl function and return a mupl
; function that takes a mupl list and applies the function to every element of the list returning a
; new mupl list. Recall a mupl list is aunit or a pair where the second component is a mupl list.
(define mupl-map 
  (fun #f "func"
       (fun "mapr" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair 
                      (call (var "func") (fst (var "func")))
                      (call (var "mapr") (snd (var "lst")))))
            )))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
