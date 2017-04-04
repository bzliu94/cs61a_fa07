; custom responses (i.e., not based on wikipedia article)

; 2.

; ex. 2.6

(define true
  (lambda (a b)
    a))

(define false
  (lambda (a b)
    b))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; (add-1 zero)
; ->
; (lambda (f)
;   (lambda (x)
;     (f ((zero f) x))))
; ->
; (lambda (f)
;   (lambda (x)
;     (f ((lambda (w) w) x))))
; ->
; (lambda (f)
;   (lambda (x)
;     (f x)))
; (one layer of f)

; (add-1 (add-1 zero))
; ->
; (add-1 one)
; ->
; (lambda (f)
;   (lambda (x)
;     (f ((one f) x))))
; ->
; (lambda (f)
;   (lambda (x)
;     (f ((lambda (w) (f w)) x))))
; ->
; (lambda (f)
;   (lambda (x)
;     (f (f x))))

(define (add n1 n2)
  (lambda (f)
    (lambda (x)
      ((n1 f) ((n2 f) x)))))

(define three (add one two))

; works because of how composed f's work and super-charging f

; we can imagine substituting in for f a nesting of f's

; also works because we are dealing with integers and
; repeated f application, while it does the same thing
; as repeated adding, does not actually involve
; call to "add"

(define (multiply m n)
  (lambda (f)
    (m (n f))))

(define four (multiply two two))

; another way to interpret is x is base case and
; f we use to recurse and visit each layer

; plug one numeral into other's f

(define (exp n_b n_p)
  (n_p n_b))

(define eight (exp two three))

; wikipedia approach for sub-1 and prof. harvey's recommended approach 
; involve O(n) time for sub-1 and O(n ^ 2) time for sub

; using list as a stent

(define (get-smaller-cn-from-list l)
  (get-smaller-cn-from-list-helper l))

(define (get-smaller-cn-from-list-helper l)
  ((lambda (m) ; this lambda is defining get-smaller-cn-from-list-helper
     ((lambda (f) (f f m))
      (lambda (f m)
	(my-if (kk-is-nil? m)
	       (lambda () zero)
	       (lambda ()
		 (my-if (kk-is-nil? (kkdr m))
				 (lambda () zero)
				 (lambda () (add-1 (f f (kkdr m))))))))))
   l)) ; argument m

(define (sub-1 n)
  (lambda (f)
    (lambda (x)
      (let ((l ((n ; prepare a list
		 (lambda (v) ; a different f
		   ; (lambda (w) ; a different x
		     ; note that we ignore w
		     (kkons true v false))) kkons-nil)))
	(let ((cn ; prepare a result church numeral n - 1
	       (get-smaller-cn-from-list l)))
	  ((cn f) x))))))

(define (subtract n1 n2)
  ((n2 sub-1) n1))

; list structure using lambda for sub-1,
; by advice of prof. harvey

; from ex. 2.4

(define (kons x y)
  (lambda (m) (m x y)))

(define (kar z)
  (z (lambda (p q) p)))

(define (kdr z)
  (z (lambda (p q) q)))

(define my-nil true)

; these two-pair-per-list-node methods are defined in terms of one-pair-per-list-node methods, so it is in agreement with prof. harvey's suggestions

(define (kkons x y is-nil)
  (kons is-nil (kons x y)))

(define kkons-nil
  (kons true true))

(define (kkar z)
  (kar (kdr z)))

(define (kkdr z)
  (kdr (kdr z)))

; requires church boolean or predicate value argument;
; assumes nil is church false
(define (kk-is-nil? z)
  (kar z))

(define (my-if p a-wrapped b-wrapped)
  ((lambda (result-wrapped)
     (result-wrapped)) (p a-wrapped b-wrapped)))

; convert church to ordinary number
(define (try-church-num num)
  ((num (lambda (x) (+ x 1))) 0))

(define (try-church-bool b)
  (b #t #f))

