; 1.

(delay (+ 1 27))
; type is promise

(force (delay (+1 27)))
; type is numeric

; 2.

; stream-cdr only operates on promises; composing by using stream-cdr twice leads to stream operation used with non-promise, which causes the error

; 3.

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(delay (enumerate-interval 1 3))
(stream-enumerate-interval 1 3)

; the difference is that the first expression delays evaluation of whole list; 
; the second expression partially reveals the list by way of cons-stream

; 4a.

(define (num-seq n)
  (num-seq-helper n))

; assume n is a positive integer
(define (num-seq-helper n)
  (if (= n 1)
      '(1)
      (let ((m (modulo n 2)))
	(let ((next_n
	      (if (= m 1)
		  (+ (* 3 n) 1)
		  (/ n 2))))
	  (cons n (num-seq-helper next_n))))))

(define (stream-num-seq n)
  (stream-num-seq-helper n))

; assume n is a positive integer
(define (stream-num-seq-helper n)
  (if (= n 1)
      (cons-stream 1 the-empty-stream)
      (let ((m (modulo n 2)))
	(let ((next_n
	       (if (= m 1)
		   (+ (* 3 n) 1)
		   (/ n 2))))
	  (cons-stream n (stream-num-seq-helper next_n))))))

; 4b.

(define (stream-seq-length seq-stream)
  (stream-seq-length-helper seq-stream))

(define (stream-seq-length-helper curr-stream)
  (let ((curr-element (stream-car curr-stream)))
    (if (equal? curr-element 1)
	1
	(+ 1 (stream-seq-length-helper (stream-cdr curr-stream))))))

