;; Implementation of parallel-execute using call/cc.
;;
;; By Ben Rudiak-Gould, 10/2002.
;;
;; Requires STk (for "procedure-body" and first-class environments).

; uncode, procedure-body, environments are supported by STk

; regarding non-intrusive approach -- we may have a bug s.t. the problem is off-by-one; if we try to dequeue and the queue is empty but we have a most-recently-popped-process that we have delayed adding to the queue, occasionally we will fail because the queue if empty should fall back to the item we delayed adding as result for dequeue as opposed to crash; when we start to enable pausing for e.g. env-lookup-raw, then such a situation could arise and we may crash

; we note that with a buggy non-intrusive use of process queue we do not crash when we only have context switch for test-and-set!; when we have context switches at other locations, we do generally crash if we have buggy non-intrusive approach

; added and requires queue module from SLIB
(define process-queue (make-queue))
(define most-recently-popped-process (list nil))
(define (FIFO-select)
  (begin
    (if (queue-empty? process-queue)
      (begin
        most-recently-popped-process)
      (let ((past-process (dequeue! process-queue)))
        (begin
          (if (continuation? (car most-recently-popped-process)) 
            (enqueue! process-queue most-recently-popped-process))
          (set! most-recently-popped-process past-process)
          past-process)))))

(define call/cc call-with-current-continuation)

(define (parallel-execute . thunks)
  (apply run-concurrently-with-env
         ; random
         ; added
         FIFO-select
         (map (lambda (thunk)
                (cons (list (uncode (procedure-body thunk)))
		      (make-virtual-env (procedure-environment thunk)) ))
              thunks ))
  ; 'okay
)

(define (run-concurrently select . exprs)
  (apply run-concurrently-with-env
	 select
	 (map (lambda (x)
		(cons x (make-virtual-env (global-environment))) )
	      exprs )))

; having repeated liveness check for threads means we are slower than we could be

; this is heavily modified
(define (run-concurrently-with-env select . exprs-with-envs)
  (let ((threads
	 (map (lambda (exp-env)
		; there is a gratuitous wrapping list for each "thread"
		(list (call/cc ; primary continuation re-enters inside list
		       (lambda (cont) ; secondary continuation re-enters at let-bind
			 (let ((scheduler (call/cc (lambda (x) (cont x)))))
			   (scheduler (myeval (car exp-env)
					      (cdr exp-env)
					      scheduler )))))))
			   ; myeval represents the value for a call of a given procedure
	      exprs-with-envs )))
    (begin
      ; remember that threads are each wrapped in a list; line below is added
      (for-each (lambda (x) (begin (enqueue! process-queue x))) threads)
      (let loop ()
        (let ((active-threads
               (filter (lambda (x) (continuation? (car x))) threads) ))
	  (if (null? active-threads)
	      ; when we are done with active threads, 
	      ; give list of return values for all original threads
	      (map car threads)
              (let ((active (select)))
		; passing to a secondary (or later) continuation 
		; a cont. that re-enters inside a set-car!
		(set-car! active (call/cc (lambda (x) ((car active) x))))
		(loop) )))))))

(define (make-virtual-env real-env)
  (cons
   `((quote    **macro** ,macro-quote)
     (lambda   **macro** ,macro-lambda)
     (let      **macro** ,macro-let)
     (set!     **macro** ,macro-set!)
     (define   **macro** ,macro-define)
     (if       **macro** ,macro-if)
     (cond     **macro** ,macro-cond)
     (and      **macro** ,macro-and)
     (or       **macro** ,macro-or)
     (set-car! **prim**  ,prim-set-car!)
     (set-cdr! **prim**  ,prim-set-cdr!)
     (begin    **prim**  ,prim-begin)
     (test-and-set! **prim** ,prim-test-and-set!)
     ; this is added
     (increment4 **prim**, prim-increment4 ))
   real-env ))

(define (env-lookup-raw sym env scheduler)
  (call/cc scheduler)
  (let ((virtual (assq sym (car env))))
    (if virtual
        (cdr virtual)
        (eval sym (cdr env)) )))

(define (env-lookup sym env scheduler)
  (let* ((val (env-lookup-raw sym env scheduler))
         (proc-body (procedure-body val)) )
    (if (and proc-body (not (eq? (cadr proc-body) '**args**)))
        (myeval (uncode proc-body)
                (make-virtual-env (procedure-environment val))
                scheduler )
        val )))

(define (env-set! sym val env scheduler)
  (call/cc scheduler)
  (let ((virtual (assq sym (car env))))
    (if virtual
        (set-cdr! virtual val)
        (eval `(set! ,sym ',val) (cdr env)) )))

(define (env-define! sym val env scheduler)
  (call/cc scheduler)
  (set-car! env (cons (cons sym val) (car env))) )

(define (get-special-form name env scheduler)
  (if (symbol? name)
      (let ((val (env-lookup-raw name env scheduler)))
        (if (and (pair? val) (eq? (car val) '**macro**))
            val
            #f ))
      #f ))

(define (myeval expr env scheduler)
  (cond ((pair? expr)
         (let ((special (get-special-form (car expr) env scheduler)))
           (if special
               ((cadr special) (cdr expr) env scheduler)
               (let ((evaluated (eval-seq expr env scheduler)))
                 (myapply (car evaluated) (cdr evaluated) scheduler) ))))
        ((symbol? expr)
	 (env-lookup expr env scheduler) )
        (else (eval expr)) ))

(define (eval-seq exprs env scheduler)
  (if (null? exprs)
      '()
      (let ((val (myeval (car exprs) env scheduler)))
	(cons val (eval-seq (cdr exprs) env scheduler)) )))

(define (myapply func args scheduler)
  (cond ((procedure? func)
         (apply func args) )
        ((and (pair? func) (eq? (car func) '**prim**))
         ((cadr func) args scheduler) )
        ((and (pair? func) (eq? (car func) '**macro**))
         ((cadr func) (map (lambda (x) (list 'quote x)) args) scheduler) )
        (else (error "apply of non-procedure" func args)) ))

(define (make-call-environment params args env)
  (cons (let loop ((params params) (args args))
          (cond ((pair? params)
                 (cons (cons (car params) (car args))
                       (loop (cdr params) (cdr args)) ))
                ((null? params)
                 (car env) )
                (else (cons (cons params args) (car env))) ))
        (cdr env) ))

(define (macro-lambda args env scheduler)
  (let ((params (car args))
        (body (cdr args)) )
    (lambda **args**
      (let ((new-env (make-call-environment params **args** env)))
        (last (map (lambda (x) (myeval x new-env scheduler)) body)) ))))

(define (macro-let args env scheduler)
  (let ((vars (map car (car args)))
        (vals (map cadr (car args)))
        (body (cdr args)) )
    (myeval `((lambda ,vars ,@body) ,@vals) env scheduler) ))

(define (macro-define args env scheduler)
  (if (pair? (car args))
      (macro-define `(,(caar args) (lambda ,(cdar args) ,@(cdr args)))
		    env scheduler )
      (let ((val (myeval (cadr args) env scheduler)))
        (env-define! (car args) val env scheduler) )))

(define (macro-set! args env scheduler)
  (let ((val (myeval (cadr args) env scheduler)))
    (env-set! (car args) val env scheduler) ))

(define (macro-quote args env scheduler)
  (car args) )

(define (macro-if args env scheduler)
  (if (myeval (car args) env scheduler)
      (myeval (cadr args) env scheduler)
      (if (pair? (cddr args))
	  (myeval (caddr args) env scheduler)
	  'okay )))

(define (macro-cond args env scheduler)
  (cond ((null? args) 'okay)
        ((or (eq? (caar args) 'else)
             (myeval (caar args) env scheduler) )
         (car (last-pair (eval-seq (cdar args) env scheduler))) )
        (else (macro-cond (cdr args) env scheduler)) ))

(define (macro-and args env scheduler)
  (if (null? args)
      #t
      (let ((val (myeval (car args) env scheduler)))
        (if (null? (cdr args))
            val
            (and val (macro-and (cdr args) env scheduler)) ))))

(define (macro-or args env scheduler)
  (if (null? args)
      #f
      (let ((val (myeval (car args) env scheduler)))
        (if (null? (cdr args))
            val
            (or val (macro-or (cdr args) env scheduler)) ))))

(define (prim-set-car! args scheduler)
  (call/cc scheduler)
  (apply set-car! args) )

(define (prim-set-cdr! args scheduler)
  (call/cc scheduler)
  (apply set-cdr! args) )

(define (prim-begin args scheduler)
  (car (last-pair args)) )

(define (prim-test-and-set! args scheduler)
  (call/cc scheduler)
  (test-and-set! (car args)) )

(define (test-and-set! x)
  (let ((oldval (car x)))
    (set-car! x #t)
    oldval ))

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst)) ))

; this is added
(define (prim-increment4 args scheduler)
  (increment4 scheduler))

(load "includes/serial.scm")
