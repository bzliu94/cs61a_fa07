; 2019-08-27

; group #4 -- use FIFO thread selection and expensive alive-based filters -- uses concurrent.scm

; advantages:
; - is mostly self-contained except for use of continuations

; disadvantages:
; - harder to perform manual context switches
; - it is slower than groups one and two because even though we avoid expensive active-based filter; this is due to overhead from thread scheduler

; important:
; - because we use FIFO thread selection instead of random, we get same clobbering behavior as with groups #1 and #2; expected value for variable we increment for n = 300 is back to ~155 instead of ~25

; notes:
; - we have non-intrusive and intrusive ways to get 
;   non-random-based thread selection for thread scheduler
; - we are using continuations to get green threads but do not actually use more than one core

; non-intrusive
(load "includes/concurrent_modified1.scm")
; intrusive
; (load "includes/concurrent_modified2.scm")
(load "includes/serial.scm")

; this is for general use

; we might want to know the size of the process queue, 
; in which case the following method can help
(define (listify-process-queue)
  (let ((lst (queue:first-pair process-queue)))
    lst))

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


