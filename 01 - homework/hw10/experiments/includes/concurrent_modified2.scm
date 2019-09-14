;; Implementation of parallel-execute using call/cc.
;;
;; By Ben Rudiak-Gould, 10/2002.
;;
;; Requires STk (for "procedure-body" and first-class environments).

; uncode, procedure-body, environments are supported by STk

; added and requires queue module from SLIB
(define process-queue (make-queue))
(define (FIFO-initialize threads)
  (for-each (lambda (x) (begin (enqueue! process-queue x))) threads))
(define (FIFO-pop)
  (dequeue! process-queue))
(define (FIFO-push process)
  (enqueue! process-queue process))
(define (FIFO-empty?)
  (queue-empty? process-queue))

(define call/cc call-with-current-continuation)

(define (parallel-execute . thunks)
  (apply run-concurrently-with-env
         ; random
         (map (lambda (thunk)
                (cons (list (uncode (procedure-body thunk)))
		      (make-virtual-env (procedure-environment thunk)) ))
              thunks ))
  ; we want all the return values, so we comment this out
  ; 'okay
)

(define (run-concurrently . exprs)
  (apply run-concurrently-with-env
	 ; select
	 (map (lambda (x)
		(cons x (make-virtual-env (global-environment))) )
	      exprs )))

; how are continuations specifically used for thread scheduler?

; each cell holds a thread (i.e. a continuation), let-bind to name "scheduler" stores in cell s.t. we replace original continuation with next continuation by in a sense ratcheting; this second version of continuation is what exists in each cell before any thread is run; we run that second version of a continuation by providing it as an argument to a call/cc call (via e.g. "(call/cc thread)" or "(call/cc (car cell))"), so that where you invoke the thread from becomes the value of name "scheduler" that we return to; where we invoke the threads is generally from the driver loop

; (call/cc thread) ~= (call/cc (lambda (x) (thread x)))

; for when a thread is run by invoking secondary continuation via a "context switch" (via providing it as an argument to call/cc), we care about return value; the effect is that we return to that calling thread (i.e. the driver loop) early or when we totally finish a provided procedure; for former case, a continuation is returned that corresponds to where we interrupt the current thread; for latter case, the return is assumed to be a non-continuation and is result for finishing a provided procedure

; (scheduler (myeval (car exp-env) (cdr exp-env) scheduler))
; => (call/cc (car active)) ~= (eventually) result of (myeval (car exp-env) (cdr exp-env))

; continuations don't die on their own; if we call scheduler before when we do for result of myeval, we pause the current thread by updating a continuation in a cell and move to next thread

; if we have n input threads, we have at least n iterations of driver loop; this loop updates a cell when we pause a thread by assigning a new continuation for that cell; we can check this by experimenting with logging when we call (set-car! active ...)

; it makes sense that the arbiter-based approach is from 2003 and this approach is from 2002; we never finish in less than n driver loop iterations; if we never call (call/cc scheduler) for a given thread, we always finish its associated continuation and a cell has assigned to it return value for associated procedure (which we assume is not a continuation); otherwise, we update the cell to have continuation corresponding to where we paused (i.e. at that location where we call (call/cc scheduler))

; in a sense, we do not even need concept of a thread chain; when we call (call/cc scheduler) we immediately pause the current thread (by updating a cell) and move to next selected thread

; to go from non-intrusive to intrusive, we need to split up process-queue-related logic and we can even speed the algorithm up by avoiding repeated liveness check for threads

; we occasionally comment out context switches that are not for test-and-set! to simplify analysis of sub-case four in group four, but parallel-execute still works if we have all the context switches uncommented out

; we understand how the scheduler works -- everytime we pause we add a new dog-ear and move on to next thread -- we repeat until all continuations fall-through by being replaced with non-continuations

; note that a weakness is that we assume that values returned from procedures passed to parallel-execute do not return continuations -- otherwise, we will run them until we get a return for that continuation that is not a continuation s.t. we can have infinite loop (e.g. if we have return value be result of (call/cc (lambda (k) k)))

; continuations are about a "diverting" method and a "re-enter" location

; this is heavily modified
(define (run-concurrently-with-env . exprs-with-envs)
  (let ((threads
	 (map (lambda (exp-env)
		; there is a gratuitous wrapping list for each "thread" that we stick to
		(list (call/cc ; primary continuation re-enters inside list
		       (lambda (cont) ; secondary continuation re-enters at let-bind
			 (let ((scheduler (call/cc (lambda (x) (cont x)))))
			   (scheduler (myeval (car exp-env)
					      (cdr exp-env)
					      scheduler )))))))
			   ; myeval represents the value for a call of a given procedure -- 
			   ; also, if we make it to this (scheduler (myeval ...)), we have 
			   ; dodged/finished intermediate (call/cc scheduler) pause calls 
			   ; and we return it to driver loop to assign as replacement 
			   ; for an active thread's continuation; also, 
			   ; scheduler is so named because using it brings us back 
			   ; to driver loop -- specifically set-car!;
	      exprs-with-envs )))
    (begin
      ; remember that threads are each wrapped in a list
      (FIFO-initialize threads)
      (let loop ()
	(if (FIFO-empty?)
	    ; when we are done with active threads, 
	    ; give list of return values for all original threads
	    (map car threads)
	    (let ((active (FIFO-pop)))
	      ; passing to a secondary (or later) continuation 
	      ; a cont. that re-enters inside a set-car!
	      (set-car! active (call/cc (lambda (x) ((car active) x))))
	      (if (continuation? (car active))
		  (FIFO-push active))
	      (loop) ))))))

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
