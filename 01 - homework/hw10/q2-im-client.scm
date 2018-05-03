; 2.

; (this file replaces the client)

(load "im-client.scm")

(define (im-broadcast message)
  ;;;Send message to who.
  (if (not
       (send-request (make-request whoiam nil 'send-msg-broadcast message) port-to-server))
      (close-connection)))

; this is a hack to easily have more than one client connect from the same machine
(define whoiam (string->word (string-append (getenv "USER") (number->string (random 100)))))


