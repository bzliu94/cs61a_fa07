;; Scheme calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond
   ((self-evaluating? exp) exp)
   ((special-word? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
   ; ((number? exp) exp)
   ((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
   (else (error "Calc: bad expression:" exp))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	((boolean? exp) true)
	((word? exp) true)
	(else false)))

(define (special-word? exp)
  (cond ((first-exp? exp) true)
	((butfirst-exp? exp) true)
	((last-exp? exp) true)
	((butlast-exp? exp) true)
	((word-exp? exp) true)))

(define (first-exp? exp)
  (equal? exp 'first))

(define (butfirst-exp? exp)
  (equal? exp 'butfirst))

(define (last-exp? exp)
  (equal? exp 'last))

(define (butlast-exp? exp)
  (equal? exp 'butlast))

(define (word-exp? exp)
  (equal? exp 'word))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (accumulate + 0 (cdr args))))))
	((eq? fn '*) (accumulate * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (accumulate * 1 (cdr args))))))
	((eq? fn 'first) (first (car args)))
	((eq? fn 'butfirst) (butfirst (car args)))
	((eq? fn 'last) (last (car args)))
	((eq? fn 'butlast) (butlast (car args)))
	((eq? fn 'word) (word args))
	(else (error "Calc: bad operator:" fn))))

