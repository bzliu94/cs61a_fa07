; 2019-08-08

; group #2 -- use arbiters, no alarm, fixed test-and-set! -- uses berkeley.scm

; advantages:
; - test-and-set! behaves correctly and we are clean s.t. we do not have alarms

; disadvantages:
; - we require many fixes to berkeley.scm

; notes:
; - sub-case four does not require test-and-set!; 
;   it is purely an experiment in clobbering by way 
;   of dealing directly with process scheduler

; important:
; - serializer is implemented the way it is in SICP (i.e. via make-mutex); 
;   this means that we are reducing to test-and-set!; each of these serializers 
;   shares an arbiter; we have one resource and there is no deadlock

; have four sub-cases: 
; 1. do not use parallel-execute and perform methods one-by-one
; 2. use parallel-execute with single serializer
; 3. use parallel-execute with many serializers
; 4. use parallel-execute with arbitrary clobbering

; conclusion:
; same as group #1 except with alarm off and test-and-set! fixed; 
; have bottleneck that same arbiter is used for all test-and-set! calls

; ideal value for n for each sub-case: 300

(load "includes/berkeley.scm")
(load "includes/serial.scm")

; fix arbiter-related test-and-set! bug by swapping then and else clauses for if

(define test-and-set!
	     (let ((arb (make-arbiter 'scratchnsniff)))
	       (lambda (cell)
		 (if (try-arbiter arb) ; had fix here
		     (let ((result (car cell)))
		       (set-car! cell #t)
		       (release-arbiter arb)
		       result)
		     (begin (process:schedule!)
			    (test-and-set! cell))))))

; we turn off alarms; alarm-interrupt is originally from SLIB multi-processing module process.scm

(define (alarm-interrupt)
  ; (alarm 1) ; don't chain alarms
  (if ints-disabled (set! alarm-deferred #t)
      (process:schedule!)))

(define (alarm-signal-handler sig)
  (alarm-interrupt))

(set-signal-handler! 14 alarm-signal-handler)

; we didn't really need the above code to turn off alarms; parallel-execute is from berkeley.scm

(define (parallel-execute . thunks)
	     (for-each (lambda (thunk) (add-process! (lambda (foo) (thunk))))
		       thunks)
	     ; (alarm-interrupt) ; set off the chained alarms
	     (process:schedule!))

; we want to clear the process queue between sub-cases; process:queue is from SLIB process.scm

(define (clear-process-queue)
  (set! process:queue (make-queue)))

; for sub-case four, we want to force killing a thread

(define (process:schedule!-no-keep)
  (defer-ints)
  (cond ((queue-empty? process:queue) (allow-ints)
				      'still-running)
	(else (call-with-current-continuation
	       (lambda (cont)
		 ; (enqueue! process:queue cont) ; don't re-insert thread into active thread collection
		 (let ((proc (dequeue! process:queue)))
		   (allow-ints)
		   (proc 'run))
		 (kill-process!))))))

; we might want to know the size of the process queue, in which case the following method can help

(define (listify-process-queue)
  (let ((lst (queue:first-pair process:queue)))
    lst))

; this is for general use

(define (getFilledList numTimes value)
  (if (eq? numTimes 0)
      nil
      (append (list value) (getFilledList (- numTimes 1) value))))

; sub-case #1 -- do not use parallel-execute and perform methods one-by-one

(define x 0)

(define (increment1)
  (set! x (+ x 1)))

(define methodList1
  (map (lambda (x) increment1)
       (getFilledList 300 0)))

(time (for-each (lambda (x) (x)) methodList1))

(display x)
(display "\n")

; sub-case #2 -- use parallel-execute with single serializer

(define y 0)

(define (increment2)
  (set! y (+ y 1)))

(define serializer2 (make-serializer))

(define serializedIncrement (serializer2 increment2))

(define methodList2
  (map (lambda (x) serializedIncrement)
       (getFilledList 300 0)))

(time (apply parallel-execute methodList2))

; (alarm-interrupt)

(clear-process-queue)

; (stop)

(display y)
(display "\n")

; sub-case #3 -- use parallel-execute with many serializers

(define z 0)

(define (increment3)
  (set! z (+ z 1)))

(define serializer3 (make-serializer))

(define methodList3
  (map (lambda (x) (serializer3 increment3))
       (getFilledList 300 0)))

(time (apply parallel-execute methodList3))

; (alarm-interrupt)

(clear-process-queue)

; (stop)

(display z)
(display "\n")

; sub-case #4 -- use parallel-execute with arbitrary clobbering

(define w 0)

(define (increment4)
  (let ((w-old w)
	(randInt (random 2)))
    (begin
      (if (equal? randInt 1)
	  (begin
	    (process:schedule!)
	    (set! w (+ w-old 1))
	    ; this "parameterized" trailing call to scheduler is important
	    (process:schedule!-no-keep)
	  )
	  (begin
	    (set! w (+ w-old 1))
	    ; this "parameterized" trailing call to scheduler is important
	    (process:schedule!-no-keep)
	    )))))

(define (getRange x)
  (if (equal? x 0)
      nil
      (append (getRange (- x 1)) (list (- x 1)))))

(define methodList4
  (map (lambda (x) increment4)
       (getRange 300)))

(time (apply parallel-execute methodList4))

; (alarm-interrupt)

(clear-process-queue)

; (stop)

(display w)
(display "\n")


