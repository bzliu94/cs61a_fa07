; 5.

; ex. 3.27

; use table for memoization

; we show how this reduces work for finding nth fibonacci number fib(n) 
; from exponential in n to linear in n

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

; (trace memo-fib)
; (memo-fib 3)
; (memo-fib 5)
; (memo-fib 7)

; (trace fib)
; (fib 3)
; (fib 5)
; (fib 7)

; (display (fib 3))
; (display (memo-fib 3))
; (display "\n")

; before we attempt to answer the questions, we first
; point out that memo-fib is defined not as a procedure, 
; but as a standard value, which means what it defines to 
; is evaluated at define time, as opposed to what happens
; with a procedure (i.e. body would then not be evaluated
; until a call to the procedure)

; a) draw environment diagram for (memo-fib 3)

; see "ex. 3.27 a.png"

; as per assignment instructions, we technically
; don't need to draw the environment diagram;
; also, we treat lookup and insert! as primitive; 
; assuming those procedures work in constant time, 
; we then want to use trace to show
; number of times memo-fib is invoked

; see "ex. 3.27.log" to see (memo-fib 3) trace

; (fib 3) causes fib to be called five times
; (memo-fib 3) causes memo-fib to be called five times

; (fib 5) causes fib to be called 15 times
; (memo-fib 5) causes memo-fib to be called 9 times

; (fib 7) causes fib to be called 41 times
; (memo-fib 7) causes memo-fib to be called 13 times

; b) why is time O(n) and not O(2 ^ n)?

; consider dependency graph

; two issues contribute to this time: 
; i) there are number of nodes linear in n
; ii) in this directed graph, we have no directed cycles and so
;     topological ordering is possible and dictates the order 
;     in which we determine node values for memoization

; c) would setting memo-fib to be (memoize fib) work?

; no, because we would only memoize once per unique query, 
; which is far too late for us to see a speed-up using, 
; unless we have a huge number of queries, in which case 
; we could still be far slower than we could be


