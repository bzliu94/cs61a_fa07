; 1.

(define-class (person name)
  (instance-vars (msg nil))
  (method (say stuff)
	  (set! msg stuff)
	  stuff)
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name)))
  (method (repeat) msg))

; 2.

(define-class (double-talker1 name)
  (parent (person name))
  (method (say stuff) (se (usual 'say stuff) (ask self 'repeat)))) 

; when repeating, says once

(define-class (double-talker2 name)
  (parent (person name))
  (method (say stuff) (se stuff stuff)))

; does not record message for repeating

(define-class (double-talker3 name)
  (parent (person name))
  (method (say stuff) (usual 'say (se stuff stuff))))

; when repeating, says twice

; probably the third version is closest to intent;
; we double-talk when we repeat as well

