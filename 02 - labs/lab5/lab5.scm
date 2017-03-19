; 1.

; ex. 2.25

(define a '(1 3 (5 7) 9))
(define b '((7)))
(define c '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr a)))))
(car (car b))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

; ex. 2.53

; (list 'a 'b 'c)
; -> (a b c)

; (list (list 'george))
; -> ((george))

; (cdr '((x1 x2) (y1 y2)))
; -> ((y1 y2))

; (cadr '((x1 x2) (y1 y2)))
; -> (car (cdr '((x1 x2) (y1 y2))))
; -> (car '((y1 y2)))
; -> (y1 y2)

; (pair? (car '(a short list)))
; -> (pair? "a short list")
; -> #f

; (memq 'red '((red shoes) (blue socks)))
; -> #f

; (memq 'red '(red shoes blue socks))
; -> (red shoes black socks)

; 2.

; ex. 2.55

; (car ''abracadabra) -> quote

; (car ''abracadabra)
; -> (car (quote abracadabra))
; -> quote

; the first quote is necessary to prepare a string literal;
; the second quote is not special and wraps argument with quote operator

; 3.

; ex. 2.27

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

(define (deep-reverse l)
  (reverse (deep-reverse-helper l)))

(define (deep-reverse-helper l)
  (if (null? l)
      nil
      (let ((element (car l))
	    (rest (cdr l)))
	(if (not (list? element))
	    (cons element (deep-reverse-helper rest))
	    (cons (deep-reverse element) (deep-reverse-helper rest))))))
	

; 4.

(define (mystery1 L1 L2)
  (cons L1 (append L2 L1)))

(define (mystery2 L1 L2)
  (list L1 (list L1 L1)))

(define (mystery3 L1 L2)
  (append (cons L2 L2) L1))

; the below implementations work as well; 
; a takeaway is that sometimes implementation
; for certain behavior is not unique

(define (mystery1b L1 L2)
  (append (cons L1 L2) L1))

(define (mystery2b L1 L2)
  (cons L1 (list (list L1 L1))))

(define (mystery3b L1 L2)
  (cons L2 (append L2 L1)))


