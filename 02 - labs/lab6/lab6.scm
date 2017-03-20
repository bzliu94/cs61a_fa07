; 1a.

; ((lambda (x) (+ x 3)) 5)

; we read in the line, we see a pair for function invocation and evaluate the operands, we read a lambda expression, we read a constant, we apply the function to the operands by substituting in arguments into variables

; 1b.

; for y-combinator, name is a slot and not just a name

((lambda (f n n0) ; this lambda is defining REDUCE
   ((lambda (reduce) (reduce reduce f n n0))
    (lambda (reduce f n n0)
      (if (null? n)
	  n0
	  (f (car n) (reduce reduce f (cdr n) n0))))))
 (lambda (x y) ; argument f
   (if (< x y)
       x
       y))
 '(1 2 3 4 5 6 2 3 4 5 6 7) ; argument n
 8) ; argument n0

; 1c.

; (map first '(the rain in spain))
; (map (lambda (x) (first x)) '(the rain in spain))

; we can't use primitive map function with our lambda because, according to scheme1.scm documentation, we store lambdas as the associated expressions, and not as full-fledged procedure objects; this implies built-in map will fail to handle lambda as expected

; 1d.

; see file scheme1_with_and.scm

; important note is that if we use eval, we get unexpected behavior;
; there is a collision in namespace - (exp x) is e ^ x and exp is a variable name

; 2.

; ex. 2.62

(define (union-set set1 set2)
  (union-set-helper nil set1 set2))

; add an element if it hasn't been seen before
(define (union-set-helper prev-element set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1)) (x2 (car set2)))
	   (cond ((and (not (equal? x1 prev-element))
		       (not (equal? x2 prev-element)))
		  (if (< x1 x2)
		      (cons x1 (union-set-helper x1 (cdr set1) set2))
		      (cons x2 (union-set-helper x2 set1 (cdr set2)))))
		 ((and (= x1 prev-element) (= x2 prev-element))
		  (union-set-helper prev-element (cdr set1) (cdr set2)))
		 ((= x1 prev-element)
		  (union-set-helper prev-element (cdr set1) set2))
		 ((= x2 prev-element)
		  (union-set-helper prev-element set1 (cdr set2))))))))
		  
; 3.

; fig 2.16

; a.

(adjoin-set 11 (adjoin-set 5 (adjoin-set 1 (adjoin-set 9 (adjoin-set 3 (adjoin-set 7 nil))))))

; b.

(adjoin-set 11 (adjoin-set 9 (adjoin-set 5 (adjoin-set 7 (adjoin-set 1 (adjoin-set 3 nil))))))

; c.

(adjoin-set 11 (adjoin-set 7 (adjoin-set 1 (adjoin-set 9 (adjoin-set 3 (adjoin-set 5 nil))))))

