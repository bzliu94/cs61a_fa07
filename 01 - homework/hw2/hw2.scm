; 1.

; ex. 1.31(a)

; product of a function at points over a given range

; define factorial in terms of product

; approximate pi using product

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

; term maps from x to y, next turns x into next x

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

; we assume n is a non-negative integer

(define (factorial n)
  (if (equal? n 0)
      1
      (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n)))

; numerator:
; x: 1, 2, 3, 4, 5, 6
; y: 2, 4, 4, 6, 6, 8

; denominator:
; x: 1, 2, 3, 4, 5, 6
; y: 3, 3, 5, 5, 7, 7

(define (quarter-pi-approx-numerator-term num-term)
  (let ((x num-term))
    (if (equal? (modulo x 2) 1)
	(+ x 1)
	(+ x 2))))

(define (quarter-pi-approx-denominator-term num-term)
    (let ((x num-term))
      (if (equal? (modulo x 2) 1)
	  (+ x 2)
	  (+ x 1))))

(define (quarter-pi-approx-numerator num-terms)
  (product quarter-pi-approx-numerator-term 1 (lambda (x) (+ x 1)) num-terms))

(define (quarter-pi-approx-denominator num-terms)
  (product quarter-pi-approx-denominator-term 1 (lambda (x) (+ x 1)) num-terms))

(define (pi-approx num-terms)
  (* 4
     (/ (quarter-pi-approx-numerator num-terms)
	(quarter-pi-approx-denominator num-terms))))

; ex. 1.32(a)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum2 term a next b)
  (accumulate + 0 term a next b))

(define (product2 term a next b)
  (accumulate * 1 term a next b))

; ex. 1.32(b)

; result for part a is recursive, so we will specify here an iterative accumulate

(define (accumulate-iter combiner null-value term a next b)
  (accumulate-iter-helper null-value combiner term a next b))

(define (accumulate-iter-helper curr-value combiner term a next b)
  (if (> a b)
      curr-value
      (accumulate-iter-helper (combiner curr-value (term a))
			      combiner term (next a) next b)))

; ex. 1.33

; implement filtered-accumulate

; filter acts on un-transformed values in [a, b]

(define (filtered-accumulate combiner null-value term a next b filter-pred)
  (filtered-accumulate-helper combiner null-value term a next b filter-pred))

(define (filtered-accumulate-helper combiner null-value term a next b filter-pred)
  (if (> a b)
      null-value
      (if (equal? (filter-pred a) #t)
	  (combiner (term a)
		    (filtered-accumulate-helper combiner null-value term (next a) next b filter-pred))
	  (filtered-accumulate-helper combiner null-value term  (next a) next b filter-pred))))

; a. sum of squares of prime numbers in interval [a, b] assuming existence of prime? predicate

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? x)
  (= n (smallest-divisor n)))

(define (result1 a b)
  (filtered-accumulate + 0 square a (lambda (x) (+ x 1)) b prime?))

; b. product of all positive integers less than n relatively prime to n (i.e. all positive integers i in [1, n) s.t. GCD(i, n) = 1

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (result2 n)
  (if (equal? n 1)
      0
      (result2-helper n)))

(define (result2-helper n)
  (let ((a 1)
	(b (- n 1)))
    (filtered-accumulate * 1 (lambda (x) x) a (lambda (x) (+ x 1)) b (lambda (x) (equal? (gcd x n) 1)))))

; ex. 1.40

; define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form:
; (newtons-method (cubic a b c) 1)
; to approximate zeros of the cubic x ^ 3 + a * x ^ 2 + b * x + c

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

; ex. 1.41

(define (double proc)
  (lambda (x)
    (proc (proc x))))

(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)
; (inc 5) -> 5 + 1 -> 6
; ((double inc) 5) -> 5 + 2 -> 7
; (((double double) inc) 5) -> 5 + 4 -> 9
; (((double (double double)) inc) 5) -> 5 + 16 -> 21

; ex. 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

; ex. 1.43

(define (repeated f n)
  (repeated-helper f n))

(define (repeated-helper f n)
  (if (equal? n 0)
      (lambda (x) x)
      (compose f (repeated-helper f (- n 1)))))

; ex. 1.46

; have two extremes - relative progress and absolute progress

(define (square x)
  (* x x))

(define (iterative-improve good-enough? improve-guess)
  (lambda (prev-guess curr-guess)
    (if (good-enough? curr-guess prev-guess)
	curr-guess
	((iterative-improve good-enough? improve-guess)
	 curr-guess
	 (improve-guess curr-guess prev-guess)))))

; be careful; reference-value can be fixed (as with sqrt) 
; or may need to change (as with fixed-point)

; improving is at worst idempotent

(define (sqrt-good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (let ((target-square x))
    (let ((proc 
	   (iterative-improve
	    (lambda (curr-guess prev-guess)
	      (sqrt-good-enough? curr-guess target-square))
	    (lambda (curr-guess prev-guess)
	      (sqrt-improve curr-guess target-square)))))
      (proc 1.0 x))))

; (sqrt 2)
; -> 1.41421568627451

(define fp-tolerance 0.00001)

(define (fp-good-enough? v1 v2)
 ; (begin
  ;   (display v1)
  ;   (newline)
  ;   (display v2)
  ;   (newline)
  (< (abs (- v1 v2)) fp-tolerance))

(define (fp-improve f guess)
  (f guess))

; do one improvement always, as otherwise we have nothing to compare a guess to

(define (fixed-point f first-guess)
  (let ((prev-guess first-guess)
	(curr-guess (fp-improve f first-guess)))
    (fixed-point-helper f prev-guess curr-guess)))

(define (fixed-point-helper f prev-guess curr-guess)
  (let ((proc
	 (iterative-improve fp-good-enough?
			    (lambda (c-guess p-guess)
			      (fp-improve f c-guess)))))
    (proc prev-guess curr-guess)))

; (fixed-point cos 1.0)
; -> 0.739082298522402

; 2.

(define (every f l)
  (every-helper f l))

(define (every-helper f l)
  (if (equal? l nil)
      nil
      (cons (f (car l)) (every-helper f (cdr l)))))

(every square '(1 2 3 4))
(every first '(nowhere man))

; 3.

(every (lambda (letter) (word letter letter)) 'purple)
; -> (pp uu rr pp ll ee)
(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
; -> (781 5 7676 909 2424)
(keep even? '(781 5 76 909 24))
; -> (76 24)
(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
; -> (o o e e e)
(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
; -> ()
(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
; -> ERROR
(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
; -> (purple)

; 4. (extra for experts)

; define fact without 'define'

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(fact 5)

; use y-combinator trick

((lambda (n) ; this lambda is defining fact
   ((lambda (fact) (fact fact n))
    (lambda (fact n)
      (if (= n 0)
	  1
	  (* n (fact fact (- n 1)))))))
    5) ; here is the argument for fact

