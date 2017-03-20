; 1.

(define (make-account balance)
  (define (withdraw amount)
    (set! balance (- balance amount)) balance)
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (dispatch msg)
    (cond 
     ((eq? msg 'withdraw) withdraw)
     ((eq? msg 'deposit) deposit)))
  dispatch)

(define (make-account2 init-amount)
  (let ((balance init-amount))
    (define (withdraw amount)
      (set! balance (- balance amount)) balance)
    (define (deposit amount)
      (set! balance (+ balance amount)) balance)
    (define (dispatch msg)
      (cond
       ((eq? msg 'withdraw) withdraw)
       ((eq? msg 'deposit) deposit)))
    dispatch))

; 2.

(define (make-account balance)
  (let ((init-balance balance))
    (define (withdraw amount)
      (set! balance (- balance amount)) balance)
    (define (deposit amount)
      (set! balance (+ balance amount)) balance)
    (define (get-balance) balance)
    (define (get-init-balance) init-balance)
    (define (dispatch msg)
      (cond 
       ((eq? msg 'withdraw) withdraw)
       ((eq? msg 'deposit) deposit)
       ((eq? msg 'balance) get-balance)
       ((eq? msg 'init-balance) get-init-balance)))
    dispatch))

; 3.

(define (make-account balance)
  (let ((init-balance balance)
	(transactions nil))
    (define (withdraw amount)
      (set! balance (- balance amount))
      (set! transactions (append transactions (list (list 'withdraw amount))))
      balance)
    (define (deposit amount)
      (set! balance (+ balance amount))
      (set! transactions (append transactions (list (list 'deposit amount))))
      balance)
    (define (get-balance) balance)
    (define (get-init-balance) init-balance)
    (define (get-transactions) transactions)
    (define (dispatch msg)
      (cond 
       ((eq? msg 'withdraw) withdraw)
       ((eq? msg 'deposit) deposit)
       ((eq? msg 'balance) get-balance)
       ((eq? msg 'init-balance) get-init-balance)
       ((eq? msg 'transactions) get-transactions)))
    dispatch))

; 4.

(define (plus1 var)
  (set! var (+ var 1))
  var)

; (plus1 5)

; substitution model
; (plus1 5)
; -> (begin (set! var (+ 5 1)) 5)
; -> (begin (set! var 6) 5)
; -> (begin 5)
; -> 5

; alternate model
; (plus1 5)
; -> var is 5, var is set to 6
; -> 6

; 5.

(define (make-adder n)
  (lambda (x) (+ x n)))

; method definition

(make-adder 3)

; lambda definition that adds three

((make-adder 3) 5)

; 5 + 3 = 8

(define (f x) (make-adder 3))

; always return a method that adds three to an input value; the x does not interfere with lambda x

(f 5)

; method that adds three to an input value

(define g (make-adder 3))

; method that adds three to an input value

(g 5)

; 5 + 3 = 8

(define (make-funny-adder n)
  (lambda (x)
    (if (equal? x 'new)
	(set! n (+ n 1))
	(+ x n))))

; method definition; if post-construction adder is passed a 'new string, 
; increment add value; otherwise, add a value to input value

(define h (make-funny-adder 3))

; lambda definition with initial adding value of three

(define j (make-funny-adder 7))

; lambda definition wiht initial adding value of seven

(h 5)

; 5 + 3 = 8

(h 5)

; 5 + 3 = 8

(h 'new)

; N/A return, adding value is incremented from three to four

(h 5)

; 5 + 4 = 9

(j 5)

; 5 + 7 = 12

(let ((a 3))
  (+ 5 a))

; 5 + 3 = 8

(let ((a 3))
  (lambda (x) (+ x 1)))

; lambda definition s.t. argument with one added is returned

((let ((a 3))
   (lambda (x) (+ x a)))
 5)

; 5 + 3 = 8

((lambda (x)
   (let ((a 3))
     (+ x a)))
 5)

; lambda that adds three to input applied to five; 
; 5 + 3 = 8

(define k
  (let ((a 3))
    (lambda (x) (+ x a))))

; method definition s.t. three is added to input

(k 5)

; 5 + 3 = 8

(define m
  (lambda (x)
    (let ((a 3))
      (+ x a))))

; method definition s.t. three is added to input

(m 5)

; 5 + 3 = 8

(define p
  (let ((a 3))
    (lambda (x)
      (if (equal? x 'new)
	  (set! a (+ a 1))
	  (+ x a)))))

; method definition; if post-construction adder is passed a 'new string, 
; increment add value; otherwise, add a value to input value; by default, value added is three

(p 5)

; 5 + 3 = 8

(p 5)

; 5 + 3 = 8

(p 'new)

; adder now adds four

(p 5)

; 5 + 4 = 9

(define r
  (lambda (x)
    (let ((a 3))
      (if (equal? x 'new)
	  (set! a (+ a 1))
	  (+ x a)))))

; method definition; if post-construction adder is passed a 'new string, 
; increment add value; otherwise, add a value to input value; by default, value added is three

(r 5)

; 5 + 3 = 8

(r 5)

; 5 + 3 = 8

(r 'new)

; adder now adds four

(r 5)

; 5 + 3 = 8; let is inside lambda with a set!

(define s
  (let ((a 3))
    (lambda (msg)
      (cond ((equal? msg 'new)
	     (lambda ()
	       (set! a (+ a 1))))
	    ((equal? msg 'add)
	     (lambda (x) (+ x a)))
	    (else (error "huh?"))))))

; method definition; if method is passed a 'new string, 
; return a lambda that allows one to increment add value; if method is passed an 'add string, return a lambda that allows one to add a value to input value; unexpected string leads to error; by default, value added is three

(s 'add)

; lambda that allows adding

(s 'add 5)

; error; only accept one argument

((s 'add) 5)

; 5 + 3 = 8

(s 'new)

; lambda to increment add value is returned

((s 'add) 5)

; 5 + 3 = 8

((s 'new))

; adder now adds four

((s 'add) 5)

; 5 + 4 = 9

(define (ask obj msg . args)
  (apply (obj msg) args))

; method definition; facilitates application of a returned lambda

(ask s 'add 5)

; 5 + 4 = 9

(ask s 'new)

; adder now adds five

(ask s 'add 5)

; 5 + 5 = 10

(define x 5)

; value definition

(let ((x 10)
      (f (lambda (y) (+ x y))))
  (f 7))

; f is a lambda that adds to x input value;
; let treats new x and f as being at same level; 
; x mentioned in f lambda is global x; 
; non-new x is 5, y is 7, so the result is 5 + 7 = 12

(define x 5)

; nothing is returned


