;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the expression (mce).

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;SECTION 4.1.1

; (define => 'arrow)

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	; added
	((and-exp? exp) 
	 (if (null? (cdr exp))
	     #t
	     (let ((curr-value (mc-eval (cadr exp) env)))
	       (if (equal? curr-value #f)
		   #f
		   (if (null? (cddr exp))
		       curr-value
		       (and curr-value (mc-eval (cons 'and (cddr exp)) env)))))))
	; added
	((or-exp? exp) 
	 (if (null? (cdr exp))
	     #f
	     (let ((curr-value (mc-eval (cadr exp) env)))
	       (if (equal? curr-value #t)
		   #t
		   (or curr-value (mc-eval (cons 'or (cddr exp)) env))))))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 ; expressions are read in as hierarchically-organized string tokens
	 ; (begin
	   (make-procedure (lambda-parameters exp)
			   (lambda-body exp)
			   env))
	 ; (display (car exp))
	 ; (display (equal? (lambda-parameters exp) '(x)))
	 ; )
	((begin? exp) 
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (mc-eval (cond->if exp) env))
	((application? exp)
	 (mc-apply (mc-eval (operator exp) env)
		   (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
	((boolean? exp) true)
	(else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

; added
(define (and-exp? exp)
  (tagged-list? exp 'and))

; added
(define (or-exp? exp)
  (tagged-list? exp 'or))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; turns a list of expression strings into one expression
(define (sequence->exp seq)
  ; (begin
  ; (display seq)
  (cond ((null? seq) seq) ; return empty list
        ((last-exp? seq) (first-exp seq)) ; return only subexpression
        (else (make-begin seq)))) ; return a begin expression
  ; )

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


; an expression is a cond if it is tagged with 'cond
(define (cond? exp) (tagged-list? exp 'cond))

; to get cond clauses, we take all but first expressions in expression
(define (cond-clauses exp) (cdr exp))

; to know if a clause is an else clause, it is tagged with 'else
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

; to get predicate for a clause, it is first expression in it
(define (cond-predicate clause) (car clause))

; to get action collection for a clause, it is list without first expression from clause; 
; yes, it is possible to have multiple actions
(define (cond-actions clause) (cdr clause))

; rewrite a cond as chained if statements
(define (cond->if exp)
  (let ((result (expand-clauses (cond-clauses exp))))
    (begin
      ; (display result)
      result)))

; hope the clause has at least two elements
(define (cond-extended-clause? clause)
  (and (not (equal? (cdr clause) nil))
       (equal? (cadr clause) '=>)))

(define (clause-has-action? clause)
  (not (equal? (cond-actions clause) nil)))

(define (cond-extended-unary-function clause)
  (caddr clause))

; need to distinguish between standard and extended cond clauses

; rewrite a cond as chained if statements
(define (expand-clauses clauses)
  ; if no clauses remain
  (if (null? clauses)
      ; according to section 1.1.6, cond with no predicates evaluating to true 
      ; leads to undefined return value; ucb scheme gives 'okay, while this approach 
      ; gives #f; 
      ; also, a clause with no result expression (including else) returns #t, 
      ; which is what ucb scheme does
      'false ; ended with no else clause; 'false is #f
      ; have clauses remaining
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
	    ; prepare to end
            (if (null? rest)
		; prepare converted else
		; (begin
		; (display (sequence->exp (cond-actions first)))
		(if (clause-has-action? first)
		    (sequence->exp (cond-actions first))
		    #t)
		; )
		; too early
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
	    ; current clause is not an else
	    ; (if 
	    ; recurse by partially preparing converted predicate-action-collection if expression
	    ; check for clause type and handle differently
	    (let ((p (cond-predicate first)))
	      (if (cond-extended-clause? first)
		  (make-if p ; predicate
			   (let ((uf (cond-extended-unary-function first)))
			     ; (begin
			       ; (display "hello")
			       ; (display uf)
			       ; (display p)
			       ; (display (equal? '(+ 1 2) p))
			       ; (display (list uf p))
			       (list uf p)) ; consequent
			     ; )
			   (expand-clauses rest)) ; alternative
		  (make-if p ; predicate
			   (if (clause-has-action? first)
			       (sequence->exp (cond-actions first)) ; consequent
			       #t)
			   (expand-clauses rest)))))))) ; alternative

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'import
                      (list 'primitive
			    (lambda (name)
			      (define-variable! name
				                (list 'primitive (eval name))
				                the-global-environment)))
                      initial-env)
    initial-env))

;[do later] (define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)
	(list 'list list)
	(list 'append append)
	(list 'equal? equal?)
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (mc-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;;Following are commented out so as not to be evaluated when
;;; the file is loaded.
;;(define the-global-environment (setup-environment))
;;(driver-loop)

;; Added at Berkeley:
(define the-global-environment '())

(define (mce)
  (set! the-global-environment (setup-environment))
  ; added
  (define-variable! '=> 'arrow the-global-environment)
  (driver-loop))

