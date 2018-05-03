; 4.

; (this file replaces the client)

(load "im-client.scm")

; (have refuse)

; this is a hack to easily have more than one client connect from the same machine
(define whoiam (string->word (string-append (getenv "USER") (number->string (random 100)))))

(define (im-refuse who)
  (if (not
       (send-request (make-request whoiam who 'do-refuse nil) port-to-server))
      (close-connection)))

(define (setup-request-handler port-from-server)
  ;;;Handle messages from the server.
  ;
  ;Only handles "receive-msg", "client-list", and "goodbye".
  ;
  (define (request-handler)
    (let ((req (get-request port-from-server)))
      (if (not req)
	  (close-connection)
	  (begin
	    (format logging "Received request: ~S~%" req)
	    (cond
	     ((equal? 'receive-msg (request-action req))
	      (received-msg (request-src req) (request-data req)))
	     ((equal? 'client-list (request-action req))
	      (update-client-list (request-data req)))
	     ((equal? 'goodbye (request-action req))
	      (close-connection))
	     ((equal? 'handle-refuse-instance (request-action req))
	      (handle-refuse-instance (request-src req)))
	     (else
	      (format #t "Unknown action requested: ~A~%" (request-action req))
	      (close-connection)))) ))
    ;; if there is more data to handle.
    (if (and (not (port-closed? port-from-server))
	     (char-ready? port-from-server))
	(request-handler)))
  ;; Now set up handler
  (when-port-readable port-from-server request-handler))

(define (handle-refuse-instance from-whom)
  (format #t "You have been refused by user ~A.~%" from-whom))


