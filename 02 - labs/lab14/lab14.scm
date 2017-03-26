; 1.

; ex. 4.27

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

; with lazy evaluation, we keep thunking as long as we get value for expression asked about, similar to with normal-order evaluation; lazy refers to evaluator, normal-order refers to language

; forcing occurs when value passed to a primitive procedure, when value is predicate of a conditional, value of an operator is about to be applied as a procedure

; (define w (id (id 10)))
; -> (define w (id 10)) with (set! count (+ 0 1)) side effect
; (note that this is following normal-order and set! and + are primitive)
; count
; -> 1
; w
; -> (define w 10) with (set! count (+ 1 1)) side effect
; count
; -> 2

; primitive expressions are strict
; compound expressions are non-strict

; ex. 4.29

(define (square x)
  (* x x))

; count starts at zero

; WITHOUT MEMOIZING:
; (square (id 10))
; -> (* (id 10) (id 10)) with set! side effect twice
; -> (* 10 10)
; -> 100
; count
; -> 2

; WITH MEMOIZING:
; (square (id 10))
; -> (* (id 10) (id 10)) with set! side-effect for first (id 10) call
; -> (* 10 10)
; -> 100
; count
; -> 1

; a program that would be much slower without memoization with normal-order evaluation would be fibonacci

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; the idea is that lazy.scm mce gives us lazy evaluation (i.e. normal-order evaluation) with memoization

; also, lazy.scm mce with modified force-it gives us lazy evaluation (i.e. normal-order evaluation) without memoization

; this is without memoization
(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(fib 25)
; without memoization - 3.727s
; with memoization - 0.035s

; used:
; time echo | stk-simply -load lazy_with_memo.scm
; time echo | stk-simply -load lazy_without_memo.scm

; 2.

; note that lazy evaluation and ambiguous evaluation have some overlap

; the code works because we avoid division by zero by strategically evaluating


