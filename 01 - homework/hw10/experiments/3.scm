; 2019-08-08

; group #3 -- use random thread selection and alive-based filters -- uses concurrent.scm

; advantages:
; - uses continuations directly, does not use multi-processing SLIB module 
;   (for process functionality) or SCM-inspired arbiters or SCM-inspired alarms; 
;   it is mostly self-contained except for use of continuations
; - requires few to no changes to concurrent.scm; if we wish to have 
;   manual context switch as for sub-case four, we must add a special form 
;   to concurrent.scm

; disadvantages:
; - harder to perform manual context switches
; - we repeatedly filter to get active threads but repeatedly start 
;   from full thread list and filter it; as a result, it is correct 
;   but slower than it could be

; notes:
; - time method hides return value

; important:
; - random thread selection has large impact on time required, 
;   so for parallel-execute cases we set n to be much lower 
;   -- we go from 300 down to 10
; - has many built-in scheduler calls by modifying many special forms, 
;   which can slow execution down
; - for sub-case four, we have more clobbering; with random thread selection policy, 
;   it is harder to get back to a thread that has already been paused -- for this reason, 
;   we have more clobbering and the expected value for the variable we increment 
;   is lower (i.e. for n = 300, we go from ~155 for groups #1 and #2 to ~25)

; have four sub-cases: 
; 1. do not use parallel-execute and perform methods one-by-one
; 2. use parallel-execute with single serializer
; 3. use parallel-execute with many serializers
; 4. use parallel-execute with arbitrary clobbering

; conclusion:
; we need to change to e.g. FIFO thread selection policy

; ideal value for n for sub-cases: 300, 10, 10, 100

(load "includes/concurrent_modified.scm")
(load "includes/serial.scm")

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
       (getFilledList 10 0)))

(time (apply parallel-execute methodList2))

(display y)
(display "\n")

; sub-case #3 -- use parallel-execute with many serializers

(define z 0)

(define (increment3)
  (set! z (+ z 1)))

(define serializer3 (make-serializer))

(define methodList3
  (map (lambda (x) (serializer3 increment3))
       (getFilledList 10 0)))

(time (apply parallel-execute methodList3))

(display z)
(display "\n")

; sub-case #4 -- use parallel-execute with arbitrary clobbering

(define w 0)

; replacements occur with procedure bodies and not for just an expression passed
(define (increment4 scheduler)
  (let ((w-old w)
	(randInt (random 2)))
    (begin
      (if (equal? randInt 1)
	  (begin
	    (call/cc scheduler)
	    (set! w (+ w-old 1))
	  )
	  (begin
	    (set! w (+ w-old 1))
	    )))))

(define (getRange x)
  (if (equal? x 0)
      nil
      (append (getRange (- x 1)) (list (- x 1)))))

(define methodList4
  (map (lambda (x) (lambda () (increment4)))
       (getRange 300)))

(time (apply parallel-execute methodList4))

(display w)
(display "\n")


