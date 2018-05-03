; 1.

; (this file replaces the client)

; note that a method exists called im that sends to one client

; send a message to a list of clients by modifying the client

; (im who message)

(load "im-client.scm")

(define (im-multiple whos message)
  ;;; Send message to multiple people
  (for-each (lambda (x) (im x message)) whos))


