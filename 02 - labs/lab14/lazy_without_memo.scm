; modified lazy.scm

;;;;LAZY EVALUATOR FROM SECTION 4.2 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm
;;;; Also includes enlarged primitive-procedures list

;;;;This file can be loaded into Scheme as a whole.
;;;;**NOTE**This file loads the metacircular evaluator of
;;;;  sections 4.1.1-4.1.4, since it uses the expression representation,
;;;;  environment representation, etc.
;;;;  You may need to change the (load ...) expression to work in your
;;;;  version of Scheme.

;;;;Then you can initialize and start the evaluator by evaluating
;;;; the expression (mce).


;;**implementation-dependent loading of evaluator file
;;Note: It is loaded first so that the section 4.2 definition
;; of eval overrides the definition from 4.1.1
(load "mceval.scm")


;;  To run without memoization, reload the first version of force-it below


;;;SECTION 4.2.2

;;; Modifying the evaluator

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (mc-apply (actual-value (operator exp) env)
		   (operands exp)
		   env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (mc-eval exp env)))

(define (mc-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (begin 
  (let ((input f1))
    ; (display input)
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (let ((input f2))
    ; (display input)
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  ; (let ((input (read)))
  (let ((input '(fib 25)))
    ; (display input)
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  ; (driver-loop))
  )
  )


;;; Representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  ; (display obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


;; memoizing version of force-it

(define (force-it obj)
  ; (display obj)
  ; (display (thunk? obj))
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define f1 '(define (fib n)
  (fib-iter 1 0 n)))

(define f2 '(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))))


;; A longer list of primitives -- suitable for running everything in 4.2
;; Overrides the list in ch4-mceval.scm

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
	(list 'number? number?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))

'LAZY-EVALUATOR-LOADED

(mce)

(exit)


