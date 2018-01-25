; 1.

; random number generator object

(load "obj.scm")

(define-class (random-generator rightValueExclusive)
  (instance-vars (count 0))
  (method (number)
	  (begin
	    (set! count (+ count 1))
	    (random rightValueExclusive))))

(define r10 (instantiate random-generator 10))

(display (ask r10 'number))

(display (ask r10 'count))

; 2.

; coke-machine object

(define-class (coke-machine capacity priceInCents)
  (instance-vars (numCokes 0) (numCents 0))
  (method (deposit amount)
	  (set! numCents (+ numCents amount)))
  (method (coke)
	  (let ((haveEnoughMoney (>= numCents priceInCents))
		(haveEnoughCokes (> numCokes 0)))
	    (cond ((eq? haveEnoughMoney #f)
		   "Not enough money")
		  ((eq? haveEnoughCokes #f)
		   "Machine empty")
		  (else
		   (let ((change (- numCents priceInCents)))
		     (set! numCokes (- numCokes 1))
		     (set! numCents 0)
		     change)))))
  (method (fill amount)
	  (set! numCokes (+ numCokes amount))))

(define my-machine (instantiate coke-machine 80 70))

(ask my-machine 'fill 60)
(ask my-machine 'deposit 25)
(display (ask my-machine 'coke))
(ask my-machine 'deposit 25)
(ask my-machine 'deposit 25)
(display (ask my-machine 'coke))

; 3.

; deck object

; ordered deck is: AH 2H 3H ... QH KH AS 2S ... QC KC

(define hearts '(AH 2H 3H 4H 5H 6H 7H 8H 9H 10H JH QH KH))

(define spades '(AS 2S 3S 4S 5S 6S 7S 8S 9S 10S JS QS KS))

(define diamonds '(AD 2D 3D 4D 5D 6D 7D 8D 9D 10D JD QD KD))

(define clubs '(AC 2C 3C 4C 5C 6C 7C 8C 9C 10C JC QC KC))

(define ordered-deck (append hearts spades diamonds clubs))

; (define ordered-deck '(AH AS))

; (define ordered-deck '(AH))

(define (shuffle deck)
  (if (null? deck)
      '()
      (let ((card (nth (random (length deck)) deck)))
	(cons card (shuffle (remove card deck))))))

(display (shuffle ordered-deck))

; (display ordered-deck)

(define-class (deck)
  (instance-vars (curr-cards ordered-deck))
  (initialize
   (set! curr-cards (shuffle curr-cards)))
  (method (empty?)
	  (eq? (length curr-cards) 0))
  (method (deal)
	  (if (ask self 'empty?)
	      nil
	      (let ((top-card (car curr-cards))
		    (remaining-cards (cdr curr-cards)))
		(set! curr-cards remaining-cards)
		top-card))))

(define d (instantiate deck))

(display (ask d 'empty?))
(display (ask d 'deal))
(display (ask d 'deal))
(display (ask d 'empty?))

; (display d)

; 4.

; miss manners object

(define-class (miss-manners wrapee)
  (method (please message argument)
	  (ask wrapee message argument))
  (default-method
    (string-append "Error: no method" " " (symbol->string message))))

(define-class (person name location)
  (method (go direction)
	  (cond ((eq? direction 'down)
		 "Brian moved from BH-office to Soda")
		((eq? direction 'east)
		 "Brian moved from BH-office to PSL"))))

(define BH (instantiate person 'Brian 'BH-office))

(define fussy-BH (instantiate miss-manners BH))

(display (ask BH 'go 'down))
(display (ask fussy-BH 'go 'east))
(display (ask fussy-BH 'please 'go 'east))

; extra

; 1.

; describe example, draw inheritance hierarchy, describe methods that each class provides, and give reason why to inherit a particular method from a second-choice parent over first-choice grandparent

; we have TA-singer and singer-TA

;        worker
;      /        \
;   TA          singer
;    |      X       |
; TA-singer    singer-TA

; below, we have classes and the methods defined specifically in those classes (and not just inherited)

; worker methods: work
; TA methods: work, grade-exam
; singer methods: sing
; TA-singer methods: N/A
; singer-TA methods: N/A

; though we can have more than two parents, we are considering just having two parents for this problem

; there is subtlety in that we might prefer method body inherited from second-choice parent over one from first-choice grandparent

; in this example, for TA-singer, primary parent is TA, secondary parent is singer, primary grandparent is worker

; in this example, for singer-TA, primary parent is singer, secondary parent is TA, primary grandparent is worker

; it is ambiguous because distance is in a sense two for both secondary parent and primary grandparent; e.g. for singer-TA, have conflict between TA (secondary parent) and worker (primary grandparent); our version of scheme apparently prefers whistling (from primary grandparent) over offering to help with box and pointer diagram (from secondary parent); we could conceivably wish to have the opposite behavior be chosen - i.e. we have offering to help with box and pointer diagram when we work for a singer-TA instance

; (define-class (C1))
; (define-class (C2))
; (define-class (C3))
; (define-class (C4)
;   (parent (C1) (C2) (C3))
;   (method (greet)
; 	  "hello to you as well"))

; (define currC4 (instantiate C4))

; (display (ask currC4 'greet))

