; 2019-08-07

; group #1 -- use arbiters and alarm -- uses berkeley.scm

; advantages:
; - requires few to no changes to berkeley.scm

; disadvantages:
; - test-and-set! is flawed and our approach would be wrong 
;   except we do have alarm-based waking of sleeping threads; 
;   for this reason, we are technically still correct in sense 
;   that eventually we get all threads to finish

; notes:
; - if we leave in alarm, handler will continue to attempt 
;   to resume processes in our process queue; this happens 
;   until one calls "stop"; if we call stop too early, 
;   however, some threads may have failed to finish and will 
;   remain paused until we somehow continue to attempt to wake
; - because we have wrong test-and-set! implementation, 
;   we get emergent pairing and with alarm turned off, 
;   it is not a coincidence that for y and z we get exactly 
;   floor of half of n (i.e. instead of 300 we get 150); 
;   we reiterate that below we have alarm turned on
; - sub-case four does not require test-and-set!; it is purely an 
;   experiment in clobbering by way of dealing directly 
;   with process scheduler
; - alarm-interrupt default definition found in slib/process.scm; 
;   alarm by default provides one second per process

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
; not much point to these results given that we are 
; interested in what happens with working test-and-set!

; ideal value for n for each sub-case: 300

(load "includes/berkeley.scm")
(load "includes/serial.scm")

; to disable alarms, uncomment:
; (define (alarm-interrupt)
;   ; (alarm 1) ; don't chain alarms
;   (if ints-disabled (set! alarm-deferred #t)
;       (process:schedule!)))
; (define (alarm-signal-handler sig)
;   (alarm-interrupt))
; (set-signal-handler! 14 alarm-signal-handler)
; (define (parallel-execute . thunks)
; 	     (for-each (lambda (thunk) (add-process! (lambda (foo) (thunk))))
; 		       thunks)
; 	     ; (alarm-interrupt) ; set off the chained alarms
; 	     (process:schedule!))

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

(stop)

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

; half the time an increment attempt will defer to other threads 
; s.t. when we return we clobber the effects of those other threads; 
; the other half of the time an increment attempt will immediately increment; 
; all of the time after incrementing the current thread is killed; 
; the overall effect is that we should have clobbering s.t. the number 
; we end up with is significantly lower than 300 (i.e. it's around 
; slightly higher than 150)

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


