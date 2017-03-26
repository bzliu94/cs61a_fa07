; 1.

; methods in metacircular evaluator that call eval or mc-eval

; mc-eval, mc-apply, list-of-values, eval-if, eval-sequence, eval-assignment, eval-definition, driver-loop, setup-environment

; 2.

; methods in metacircular evaluator that call apply or mc-apply

; mc-eval, mc-apply, apply-in-underlying-scheme, apply-primitive-procedure

; 3.

; make-procedure does not call eval because we have to have parameter values to tailor the body of the method to for eval

; 4.

; ex. 4.1

; implement list-of-values to prefer evaluating operands left-to-right, then right-to-left

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (list-of-values-l-to-r exps env)
  (if (no-operands? exps)
      '()
      (let ((left (mc-eval (first-operand exps) env)))
	(let ((right (list-of-values-l-to-r (rest-operands exps) env)))
	  (cons left right)))))

(define (list-of-values-r-to-l exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-r-to-l (rest-operands exps) env)))
	(let ((left (mc-eval (first-operand exps) env)))
	  (cons left right)))))

; ex. 4.2

; a.

; (define x 3) will look like a procedure application, but x will be undefined

; b.

; for application? we had:

(define (application? exp) (pair? exp))

; now, we have:

(define (application? exp)
  (and (pair? exp)
       (equal? (car exp) 'call)))

; ex. 4.4

; have and and or

; to test, load "mceval_modified.scm" and run (mce)

; ex. 4.5

; have cond extended to have an arrow

; to test, load "mceval_modified.scm" and run (mce)

; a cond clause is a element of cond statement; inside, there is condition and result, normally; here, we have condition and then a unary operator that acts on condition value

; we note that our implementation works when predicate is e.g. #t directly, even though (#t ...) is not a function call

; we don't have an explicit grammar; we have a simple approach, i.e. we have tags

; one might think that to have fancier syntax as with extended cond, we would need a complicated grammar; this is not the case, as long as we take care to order the checks for the cases satisfactorily

; e.g., use (cond ((+ 1 1) => (lambda (x) x))) or (cond (#t => (lambda (x) x)))

; 5a.

; see logo.log

; 5b.

; dynamic scope is look-up based on location of method call

; lexical scope is look-up based on location of method definition

; see scope.logo

; 5c.

; " (double-quote)
; [] (square bracket pair)
; : (colon)

; square brackets help us denote lists

; double-quote helps us denote strings

; according to logo manual, colons are optional and don't have any effect


