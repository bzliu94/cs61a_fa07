; 1.

; ex. 1.16

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (fast-expt-iter b n)
  (fast-expt-iter-helper b n 1))

; b ^ n = (b ^ (n / 2)) ^ 2 = (b ^ 2) ^ (n / 2)

; shuffle values with b and n, 
; shuffling tells us when to stop, 
; result is in a

; invariant is a * b ^ n

(define (fast-expt-iter-helper b n a)
  (cond ((= n 0) a)
        ; instead of square of call with b unchanged and n halved, 
        ; square b and halve n
        ((even? n) (fast-expt-iter-helper
		    (square b) (/ n 2)
		    a))
	(else (fast-expt-iter-helper
	       b (- n 1)
	       (* a b)))))

; b ^ n = (b ^ (n / 2)) ^ 2 = (b ^ 2) ^ (n / 2) if n is even
; b ^ n = b * b ^ (n - 1) if n is odd

; iterative version of fast exponentiation

; ex. 1.35

; calculate phi (golden ratio) using fixed-point procedure with
; transformation x -> 1 + 1 / x

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

(define (phi-mapping x)
  (+ 1 (/ 1 x)))

(define calculate-phi
  (fixed-point phi-mapping 1))

; ex. 1.37

; a.

; calculate an infinite continued fraction

; test using 1 / phi with N_i and D_i equal to one

; how large must k be for k-term finite continued fraction to be accurate to four decimal places?

(define (cont-frac n d k)
  (if (equal? k 0)
      0
      (cont-frac-helper n d k 1)))

; n and d are 1-indexed

(define (cont-frac-helper n d k i)
  (let ((n_i (n i))
	(d_i (d i)))
    (if (equal? k i)
	(/ n_i d_i)
	(/ n_i (+ d_i (cont-frac-helper n d k (+ i 1)))))))

(define (next-calculate-phi k)
  (let ((reciprocal (cont-frac
		     (lambda (i) 1.0)
		     (lambda (i) 1.0)
		     k)))
    (/ 1 reciprocal)))

; as reference, phi is ~1.61803398874989...

; to get accurate to four decimal places (1.6180) with truncation, k must be 12 - we get 1.61805555555556 ~= 1.6180...

; b.

(define (cont-frac-iter n d k)
  (if (equal? k 0)
      0
      (cont-frac-iter-helper n d k k 0)))

(define (cont-frac-iter-helper n d k i a)
  (let ((n_i (n i))
	(d_i (d i)))
    (if (equal? i 1)
	(/ n_i (+ d_i a))
	(cont-frac-iter-helper n d k (- i 1)
			       (/ n_i (+ d_i a))))))

; ex. 1.38

; approximate e by determining e - 2 using a continued fraction

; 1 2 3 4 5 6 7 8 9 10 11 12 13
; 1 2 1 1 4 1 1 6 1 1  8  1  1

; 2 -> 2, 5 -> 4, 8 -> 6, 11 -> 8; left is i, right is d_i
; 0 -> 2, 3 -> 4, 6 -> 6, 9 -> 8; i -> i - 2
; 0 -> 2, 1 -> 4, 2 -> 6, 3 -> 8; i -> (i - 2) / 3
; 0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4; d_i -> d_i / 2
; 0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3; d_i -> (d_i / 2) - 1

; -> (i - 2) / 3 = (d_i / 2) - 1
; -> d_i = ((i - 2) / 3 + 1) * 2

(define (calculate-e-d i)
  (let ((m (modulo i 3)))
    (cond
     ((equal? m 0) 1)
     ((equal? m 1) 1)
     ((equal? m 2) (* 2 (+ 1 (/ (- i 2) 3)))))))

(define (calculate-e k)
  (let ((intermediate-target (cont-frac
			      (lambda (i) 1.0)
			      calculate-e-d
			      k)))
    (+ intermediate-target 2)))

; (calculate-e 10)
; -> 2.71828171828172

; 2.

; perfect number is a number equal to sum of all its factors less than itself

; first perfect number is 6 = 1 + 2 + 3

; second perfect number is 28 = 1 + 2 + 4 + 7 + 14

; what is the third perfect number?

; implement next-perf method

; hint - you'll need a sum-of-factors subprocedure

; test on (next-perf 1) = 6 and (next-perf 29) for third perfect number

; (next-perf n) returns first perfect number >= n

; n >= 1

(define (next-perf n)
  (next-perf-helper n))

(define (next-perf-helper n)
  (if (equal? n (sum-of-factors n))
      n
      (next-perf-helper (+ n 1))))

(define (sum-of-factors n)
  (sum-of-factors-helper n 1))

; use brute-force

(define (sum-of-factors-helper n i)
  (cond ((>= i n) 0)
	((equal? (modulo n i) 0)
	 (+ i (sum-of-factors-helper n (+ i 1))))
	(else (sum-of-factors-helper n (+ i 1)))))

; answer is 496

; 3.

; what would happen if we interchange the order of the first two cond cases for cc

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(define (next-cc amount kinds-of-coins)
  (cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
	((= amount 0) 1)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

; the original first case is to do with considering number of combinations for no change needed, which we say is one

; the original second case is to do with pruning when we have negative target values for change combination or we have no more coin types left

; amount less than zero and amount equal to zero are disjoint cases

; so, we have to say - when does number of kinds of coins left occur at same time as amount equalling zero

; note that (cc 0 0) is one and (next-cc 0 0) is zero

; only in this one case (i.e. kinds of coins is zero and amount is zero at same time) will we get a different value (one in original, zero in modified version)

; further, amount of zero and kinds-of-coins of zero have to be for an original call and won't occur spontaneously at same time with different initial amount and kinds-of-coins values because of our two base cases which prune away branches that could lead to both amount and kinds-of-coins being zero at same time (through cond case three)

; 4.

; b, n, counter, product

; an invariant would be:
; product = b ^ (n - counter)

; extra

; 1.

(define (cc-p amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc-p amount
		     (- kinds-of-coins 1))
		 (cc-p (- amount
			(first-denomination-p kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination-p kinds-of-coins)
  kinds-of-coins)

(define (number-of-partitions n)
  (cc-p n n))

; be cautious; cc-p has to have recursive calls to cc-p, not cc

; 2.

; counting partitions is like making change, where the coins are of size in [1, n]

; 3.

; turn cc-p into an iterative method

; we can still have tree-recursion and have it count as iterative
; if we more vigorously pursue collapsing of chain of additions

; have a state variable; i.e., have a partial sum for number of combinations

(define (cc-p-iter amount kinds-of-coins)
  (cc-p-iter-helper amount kinds-of-coins 0))

(define (cc-p-iter-helper amount kinds-of-coins partial-sum)
  (cond ((= amount 0) (+ partial-sum 1)) ; a base-case
	((or (< amount 0) (= kinds-of-coins 0)) partial-sum) ; a base-case
	(else (let ((curr-sum
		     (cc-p-iter-helper
		      amount (- kinds-of-coins 1) partial-sum)))
		(cc-p-iter-helper
		 (- amount (first-denomination-p kinds-of-coins))
		 kinds-of-coins curr-sum)))))

