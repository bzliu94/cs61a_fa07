; ex. 2.7

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

; ex. 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; ex. 2.10

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (div-interval2 x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "ambiguous behavior as divisor interval spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; "spans zero" means one bound is <= zero and the other is >= zero

; ex. 2.12

; bug here in text; width ought to be twice radius

(define (make-center-width c w)
  (let ((r (/ w 2)))
    (make-interval (- c r) (+ c r))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; uncertainty/tolerance is defined to be ratio of interval width to interval midpoint

; f_tol = 2 * r / center
; => r = f_tol * center / 2

; c = 3.5, r = .15
; => f_tol = 2 * .15 / 3.5 = .3 / 3.5 ~= .08571

(define (make-center-percent c tolerance-fraction)
  (let ((r (/ (* tolerance-fraction c) 2)))
    (make-center-width c (* 2 r))))

; return a fraction

(define (percent i)
  (let ((w (width i))
	(c (center i)))
    (let ((r (/ w 2)))
      (* 2 (/ r c)))))

; ex. 2.17

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; ex. 2.20

; allow a procedure to take arbitrary number of arguments by using dotted-tail notation

; use this notation to implement a procedure same-parity that takes >= 1 arguments and returns a list of all arguments (including the first) that have same even-odd parity as first argument

(define (same-parity x . y)
  (let ((is-even (even? x)))
    (if is-even
	(cons x (filter even? y))
	(cons x (filter (lambda (x) (not (even? x))) y)))))

; ex. 2.22

; square-list takes a list of numbers and returns squares of those numbers

; debug iterative version and an attempt to fix the buggy iterative version

; first iterative version produces values in reverse order because while for both recursive and buggy iterative versions, we consider arguments from left to right and for recursive version, we accumulate left-to-right (order-preserving), but for buggy iterative version, we accumulate right-to-left

; the attempted fix doesn't work because cons must take an item as left argument and a list as right argument

; an actual fix that uses iterative approach could use append or reverse

; ex. 2.23

(define (for-each f l)
  (for-each-helper f l))

(define (for-each-helper f l)
  (if (not (equal? l nil))
      (begin
	(f (car l))
	(for-each-helper f (cdr l)))))

; 1.

; substitute is deep

(define (substitute l old-word new-word)
  (substitute-helper l old-word new-word))

(define (substitute-helper l old-word new-word)
  (if (equal? l nil)
      nil
      (let ((item (car l))
	    (next-l (cdr l)))
	(if (not (list? item))
	    (if (equal? item old-word)
		(cons new-word (substitute-helper next-l old-word new-word))
		(cons item (substitute-helper next-l old-word new-word)))
	    (cons (substitute-helper item old-word new-word)
		  (substitute-helper next-l old-word new-word))))))

; 2.

(define (substitute2 l old-word-list new-word-list)
  (substitute2-helper l old-word-list new-word-list))

(define (substitute2-helper l old-word-list new-word-list)
  (if (equal? old-word-list nil)
      l
      (let ((old-word (car old-word-list))
	    (new-word (car new-word-list))
	    (next-old-word-list (cdr old-word-list))
	    (next-new-word-list (cdr new-word-list)))
	(substitute2-helper (substitute l old-word new-word)
			    next-old-word-list
			    next-new-word-list))))

; extra

; 1.

; example of a valid argument would be 'cdddadaadar

(define (cxr-function str)
  (cxr-function-helper (bf (bl str))))

(define (cxr-function-helper trimmed-str)
  (if (equal? trimmed-str "")
      (lambda (x) x)
      (let ((curr-char (first trimmed-str))
	    (next-trimmed-str (bf trimmed-str)))
	(cond ((equal? curr-char 'a)
	       (lambda (x) (car ((cxr-function-helper next-trimmed-str) x))))
	      ((equal? curr-char 'd)
	       (lambda (x) (cdr ((cxr-function-helper next-trimmed-str) x))))))))

; 2.

; ex. 2.6

; inspired by church encoding article on wikipedia

(define zero (lambda (f) (lambda (x) x)))

; known as λn. λf. λx. f (n f x)
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; known as λm. λn. λf. λx. m f (n f x)
(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

(define three (add one two))

; intuition is that f occurs number of times roughly proportional
; to size of numeral and x is involved a constant number of times,
; independent of size of numeral

; known as λm. λn. λf. m (n f)
(define (multiply m n)
  (lambda (f)
    (m (n f))))

(define four (multiply two two))

; works best if we have small results for exponentiation

; another way to interpret is x is base case and
; f we use to recurse and visit each layer

; plug one numeral into other's f

; known as λm. λn. n m
(define (exp m n)
  (n m))

(define eight (exp two three))

; definition arrived at by introducing container abstractions and simplifying; 
; known as λn. λf. λx. n (λg. λh. h (g f)) (λu. x) (λu. u)
(define (sub-1 n)
  (lambda (f)
    (lambda (x)
      (((n (lambda (g)
	    (lambda (h)
	     (h (g f)))))
       (lambda (u)
	 x))
       (lambda (u)
	 u)))))

; known as λm. λn. (n pred) m
(define (subtract m n)
  ((n sub-1) m))

; convert church to ordinary number
(define (try-church-num num)
  ((num (lambda (x) (+ x 1))) 0))


