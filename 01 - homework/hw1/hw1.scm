; 1.

; ex. 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)
; -> 5

(new-if (= 1 1) 0 5)
; -> 0

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; infinite loop occurs because the else clause argument is evaluated even when we don't need to go down that branch

; 2.

; (square '(2 3 4 5))
; -> (4 9 16 25)

; normally, we are supposed to use sentence versions of cons, car, cdr

(define (square x)
  (* x x))

(define (squares numbers)
  (squares-helper numbers))

(define (squares-helper numbers)
  (if (equal? numbers nil)
      nil
      (cons (square (car numbers)) (squares-helper (cdr numbers)))))

; 3.

; assume characters are all lower-case

(define (switch s)
  (if (equal? s nil)
      nil
      (if (equal? (car s) 'you)
	  (cons 'i (switch-helper (cdr s)))
	  (cons (car s) (switch-helper (cdr s))))))

(define (switch-helper s)
  (if (equal? s nil)
      nil
      (cond ((or (equal? (car s) 'i)
		 (equal? (car s) 'me))
	     (cons 'you (switch-helper (cdr s))))
	    ((equal? (car s) 'you)
	     (cons 'me (switch-helper (cdr s))))
	    (else
	     (cons (car s) (switch-helper (cdr s)))))))

; 4.

; true if ascending, which we take to be non-descending

(define (ordered? numbers)
  (if (equal? numbers nil)
      #t
      (ordered?-helper (car numbers) (cdr numbers))))

(define (ordered?-helper prev-number numbers)
  (if (equal? numbers nil)
      #t
      (let ((curr-number (car numbers)))
	(if (>= curr-number prev-number)
	    (ordered?-helper curr-number (cdr numbers))
	    #f))))

; 5.

; using ucb scheme added methods from stk-simply and simply.scm; 
; namely, dealing with strings is made more straightforward
; by doing so

(define (last-letter word)
  (last word))

(define (ends-e words)
  (ends-e-helper words))

(define (ends-e-helper words)
  (if (equal? words nil)
      nil
      (let ((curr-word (car words)))
	(if (equal? (last-letter curr-word) 'e)
	    (cons curr-word (ends-e-helper (cdr words)))
	    (ends-e-helper (cdr words))))))

; 6.

; determine a test that will tell us if and/or are special forms or ordinary functions; i.e., determine whether we stop evaluating arguments early if we are certain about the output of the and/or test

(define and-is-special-form #t)

(if
 (and (= 1 0)
      (begin
	(set! and-is-special-form #f)
	(= 1 1)))
 (display "evaluates to true")
 (display "evaluates to false"))

(if (equal? and-is-special-form #t)
    (display "and is special form")
    (display "and is not special form"))

; leads to false and "and is special form"

(define or-is-special-form #t)

(if
 (or (= 1 1)
     (begin
       (set! or-is-special-form #f)
       (= 1 0)))
 (display "evaluates to true")
 (display "evaluates to false"))

(if (equal? or-is-special-form #t)
    (display "or is special form")
    (display "or is not special form"))

; leads to true and "or is special form"

; also, why would it be a good idea to treat or as a special form?

; could lead to faster code and can lead to more terse code if we know the order in which arguments are evaluated (as would be the case with early stopping) because later terms can build off of progress made for earlier terms

; why might it be a good idea to treat or as an ordinary function?

; easier implementation, possibly more predictable behavior for debugging purposes

