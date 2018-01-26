; 1.

; ex. 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch givenPassword m)
    (if (not (eq? givenPassword password))
	(lambda (x) "Incorrect password")
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))))
  dispatch)

(define acc (make-account 100 'secret-password))

(display ((acc 'secret-password 'withdraw) 40))

(display ((acc 'some-other-password 'deposit) 50))

; 2.

; ex. 3.4

(define (call-the-cops)
  (display "Cops have been called"))

(call-the-cops)

(define (make-account-alarmed balance password)
  (define consecutive-failures 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch givenPassword m)
    (if (not (eq? givenPassword password))
	(lambda (x)
	  (begin
	    (set! consecutive-failures (+ consecutive-failures 1))
	    (if (> consecutive-failures 7)
		(call-the-cops))
	    "Incorrect password"))
	(begin
	  (set! consecutive-failures 0)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request -- MAKE-ACCOUNT"
			   m))))))
  dispatch)

(define acc2 (make-account-alarmed 100 'secret-password))

(display ((acc2 'secret-password 'withdraw) 40))

(display ((acc2 'some-other-password 'deposit) 50))
(display ((acc2 'some-other-password 'deposit) 50))
(display ((acc2 'some-other-password 'deposit) 50))
(display ((acc2 'some-other-password 'deposit) 50))
(display ((acc2 'some-other-password 'deposit) 50))
(display ((acc2 'some-other-password 'deposit) 50))
(display ((acc2 'some-other-password 'deposit) 50))
(display ((acc2 'some-other-password 'deposit) 50))

; 3.

; ex. 3.7

; make-joint should allow one to access an existing password-protected bank account using a different password

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch givenPassword m)
    (if (not (eq? givenPassword password))
	(lambda (x) "Incorrect password")
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))))
  dispatch)

(define (make-joint passwordProtectedAccount oldPassword newPassword)
  (define (dispatch givenPassword m)
    (if (not (eq? givenPassword newPassword))
	(lambda (x) "Incorrect password")
	(passwordProtectedAccount oldPassword m)))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

(display ((paul-acc 'rosebud 'withdraw) 40))

; 4.

; ex. 3.8

; key is that f has state

(define (f-original x)
  (set! f (lambda (y) x))
  x)

(define f f-original)

; expect 0 + 0 = 0

(define value1
  (let ((v1 (f 0)))
    (let ((v2 (f 1)))
      (+ v1 v2))))

(display value1)

(set! f f-original)

; expect 1 + 1 = 2

(define value2
  (let ((v1 (f 1)))
    (let ((v2 (f 0)))
      (+ v1 v2))))

(display value2)

; 5.

; ex. 3.10

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-withdraw-with-let initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw-with-let-lambda initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
	   (begin (set! balance (- balance amount))
		  balance)
	   "Insufficient funds"))) initial-amount))

; i. use environment model to analyze make-withdraw-with-let-lambda and draw figures to illustrate the following three interactions

(define W1 (make-withdraw-with-let-lambda 100))

(display (W1 50))

(define W2 (make-withdraw-with-let-lambda 100))

; see "ex. 3.10.png"

; ii. show that two versions of make-withdraw create objects with same behavior

; the only difference is that the environments for differently-created make-withdraw-like objects each have an extra frame s.t. we have it store initial-amount; we have two different balance variables and associated frames as before (i.e. with make-withdraw as opposed to with make-withdraw-with-let-lambda)

; iii. how do the environment structures differ for the two versions?

; we have an extra frame for environment associated with each make-withdraw-like object that each stores a distinct initial-amount variable

; 6.

; ex. 3.11

(define (make-account-mp balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; i. show environment structure generated by the following four interactions

(define acc-mp (make-account-mp 50))

(display ((acc-mp 'deposit) 40))

(display ((acc-mp 'withdraw) 60))

(define acc2-mp (make-account-mp 100))

; see "ex. 3.11.png"

; ii. where is local state for acc-mp kept?

; balance is kept in a frame added once we call make-account; 
; method definitions are kept in that same frame

; iii. how are local states for the two accounts acc-mp and acc2-mp kept distinct?

; upon calling make-account, we add frames that point to global frame; 
; the local state for each account is in the frame that we add when 
; we call make-account for each account

; iv. which parts of environment structure are shared between acc-mp and acc2-mp?

; the environments are largely disjoint, except for
; the global frame, in which rests the make-account function 
; and the two names acc and acc2

; extra

; 1.

; unique-rename

; see "unique_rename.scm"

; miscellaneous

; an environment is a collection of frames starting at a leaf or internal frame and proceeding to global frame

; book doesn't distinguish between frames for heap vs. stack; this distinction was just introduced by a TA

; we are a lot more comfortable using internal definitions after section 3.2

