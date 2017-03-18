; 2.
; a. (cdr (car z)) -> 5
; b. (car (cons 8 3)) -> 8
; c. (car z) -> (4 . 5)
; d. (car 3) -> error

; 5.
(define (+rat a b)
  (make-rational (+ (* (numerator a) (denominator b)) (* (numerator b) (denominator a)))
    (* (denominator a) (denominator b))))

; a / b + c / d = (a * d) / (b * d) + (c * b) / (b * d) = (a * d + c * b) / (b * d)

; 6.

; ex. 2.2

(define (make-segment p_start p_end) (cons p_start p_end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (midpoint-segment s)
  (let ((p_start (start-segment s))
	(p_end (end-segment s)))
    (let ((x_start (x-point p_start))
	  (x_end (x-point p_end))
	  (y_start (y-point p_start))
	  (y_end (y-point p_end)))
      (let ((mid_x (/ (+ x_start x_end) 2))
	    (mid_y (/ (+ y_start y_end) 2)))
	(make-point mid_x mid_y)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; ex. 2.3

(define (make-segment p_start p_end) (cons p_start p_end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (midpoint-segment s)
  (let ((p_start (start-segment s))
	(p_end (end-segment s)))
    (let ((x_start (x-point p_start))
	  (x_end (x-point p_end))
	  (y_start (y-point p_start))
	  (y_end (y-point p_end)))
      (let ((mid_x (/ (+ x_start x_end) 2))
	    (mid_y (/ (+ y_start y_end) 2)))
	(make-point mid_x mid_y)))))

(define (make-rectangle1 p_ll p_ur)
  (list 'one p_ll p_ur))

(define (rectangle-lower-left r)
  (if (equal? (car r) 'one)
      (car (cdr r))
      (let ((s_l (car (cdr r))))
	(let ((p1 (start-segment s_l))
	      (p2 (end-segment s_l)))
	  (let ((x_l (x-point p1))
		(y_l (if (< (y-point p1) (y-point p2))
			 (y-point p1)
			 (y-point p2))))
	    (make-point x_l y_l))))))

(define (rectangle-upper-right r)
  (if (equal? (car r) 'one)
      (car (cdr (cdr r)))
      (let ((s_r (car (cdr (cdr r)))))
	(let ((p1 (start-segment s_r))
	      (p2 (end-segment s_r)))
	  (let ((x_r (x-point p1))
		(y_l (if (< (y-point p1) (y-point p2))
			 (y-point p2)
			 (y-point p1))))
	    (make-point x_r y_l))))))

(define (rectangle-width r)
  (let ((p_ll (rectangle-lower-left r))
	(p_ur (rectangle-upper-right r)))
    (let ((x_ll (x-point p_ll))
	  (x_ur (x-point p_ur)))
      (- x_ur x_ll))))

(define (rectangle-height r)
  (let ((p_ll (rectangle-lower-left r))
	(p_ur (rectangle-upper-right r)))
    (let ((y_ll (y-point p_ll))
	  (y_ur (y-point p_ur)))
      (- y_ur y_ll))))

(define (rectangle-perimeter r)
  (let ((width (rectangle-width r))
	(height (rectangle-height r)))
    (+ (* 2 width) (* 2 height))))

(define (rectangle-area r)
  (let ((width (rectangle-width r))
	(height (rectangle-height r)))
    (* width height)))

(define (make-rectangle2 s_l, s_r, s_u, s_d)
  (list 'two s_l s_r s_u s_d))

; ex. 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; (car (cons x y)) == x

; (car (cons x y))
; -> (car (lambda (m) (m x y)))
; -> ((lambda (m) (m x y)) (lambda (p q) p))
; -> ((lambda (p q) p) x y)
; -> x

(define (cdr z)
  (z (lambda (p q) q)))

; 8.

; ex. 2.18

; pass previous partial result; 
; at end, return grown partial result

(define (reverse l)
  (cond ((null? l) nil)
	(else
	 (let ((prev-element (car l))
	       (rest-of-list (cdr l))
	       (partial-result nil))
	   (reverse-helper partial-result rest-of-list prev-element)))))

(define (reverse-helper partial-result l prev-element)
  (cond ((null? l) (cons prev-element partial-result))
	(else
	 (let ((next-prev-element (car l))
	       (next-rest-of-list (cdr l))
	       (next-partial-result (cons prev-element partial-result)))
	   (reverse-helper next-partial-result next-rest-of-list next-prev-element)))))

