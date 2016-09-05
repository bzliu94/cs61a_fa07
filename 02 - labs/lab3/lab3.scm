(define (substitute sent old_wd new_wd)
  (substitute-helper () sent old_wd new_wd))

(define (substitute-helper next_sent sent old_wd new_wd)
  (if (empty? sent)
      next_sent
      (let ((curr_wd (first sent)))
	(let ((substituted_curr_wd (get-next-word curr_wd old_wd new_wd)))
	  (let ((next_next_sent (sentence next_sent substituted_curr_wd)))
	    (substitute-helper next_next_sent (bf sent) old_wd new_wd))))))

(define (get-next-word curr_wd old_wd new_wd)
  (if (equal? old_wd curr_wd)
      new_wd
      curr_wd))

(define (g)
  (lambda (x) 3))

;; return type is function

;; f, (f), (f 3), ((f)), (((f)) 3)

;; f - value or function taking any number of arguments
;; (f) - function taking no arguments
;; (f 3) - function taking one argument
;; ((f)) - function taking no argument that returns a function taking no argument
;; (((f)) 3) - function taking no argument that returns a function taking no argument that returns a function taking one argument

;; ((t 1+) 0) does 1+ three times
;; ((t (t 1+)) 0) does 1+ 3 * 3 times
;; (((t t) 1+) 0) does 1+ 3 ^ 3 times

;; ((t 1+) 0) = (1+ (1+ (1+ 0))) = 3
;; ((t (t 1+)) 0) = ((t (lambda (x) (1+ (1+ (1+ x))))) 0)
;; = ((lambda (y) (lambda (x) (1+ (1+ (1+ y)))))
;;     ((lambda (y) (lambda (x) (1+ (1+ (1+ y)))))
;;       (lambda (y) (lambda (x) (1+ (1+ (1+ y)))))) 0)
;; = (... (... 3))
;; = (... 6)
;; = 9
;; (((t t) 1+) 0) = ((lambda (x) (t (t (t x))) 1+) 0)
;; = (((lambda (x) (t (t (t x)))) 1+) 0)
;; = ((lambda (x) ((lambda (a) (a (a (a x)))) (lambda (b) (b (b (b x))) (lambda (c) (c (c (c x))))))) 0)
;; = (... (... (... 0)))
;; = (... (... 3))
;; = (... 9)
;; = 27

(define (t f)
  (lambda (x) (f (f (f x)))))

;; ((t s) 0) does 1+ three times
;; ((t (t s)) 0) does 1+ 3 * 3 times
;; (((t t) s) 0) does 1+ 3 ^ 3 times

;; ((t s) 0)
;; ((t (t s)) 0)
;; (((t t) s) 0)

(define (s x)
  (+ 1 x))

(define (make-tester w)
  (lambda (x) (if (equal? w x)
		  #t
		  #f)))
