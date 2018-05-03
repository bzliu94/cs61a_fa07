; 2.

; (this file replaces the server)

; send a message to all clients by modifying the server

; we note that ~% is used to end a format call because it leads to flushing

; also, loading leads to merging of namespace via global environment

(load "im-server.scm")

; we also check that the target is never the sender
(define (broadcast-to-all-clients name client-sock from-client req)
  ; we skip event and go straight to handler
  (format logging "Broadcasting a message from ~A to all.~%" name)
  (let ((client-list (get-clients-list))
	(port-from-client (socket-input client-sock)))
    (let ((next-client-list (filter (lambda (x) (not (eq? name x))) client-list)))
      (for-each (lambda (to-client)
		  (let ((target-socket (find-client-socket to-client))
			(target-name to-client)
			(message (request-data req)))
		    (let ((next-req (make-request name target-name 'send-msg message))
			  (port-to-client (socket-output target-socket)))
		      (eventless-client-request-handler
		       name client-sock port-from-client port-to-client next-req))))
		next-client-list))))

(define (eventless-client-request-handler name client-sock port-from-client port-to-client req)
  (if (not req)
      (remove-client name)
      (begin	    
	(format logging "Received request: ~S~%" req)
	(cond
	 ((equal? 'send-msg (request-action req))
	  (let ((port-to-dst (find-port-to-client (request-dst req))))
	    (if port-to-dst		   
		(begin  
		  (format logging "Delivering message from ~A to ~A.~%"
			  (request-src req)
			  (request-dst req))
		  (if (not
		       (send-request (make-request (request-src req) 
						   (request-dst req)
						   'receive-msg
						   (request-data req))
				     port-to-dst))
		      (remove-client (request-dst req))))
		(begin
		  (format logging "User not found: ~A. Letting sender know.~%"
			  (request-dst req))
		  (if (not
		       (send-request (make-request 'server
						   name
						   'receive-msg
						   (format #f "User not found: ~A"
							   (request-dst req)))
				     port-to-client))
		      (remove-client name)) ))) )
	 
	 ((equal? 'logout (request-action req))
	  (remove-client name))

	 ((equal? 'send-msg-broadcast (request-action req))
	  (broadcast-to-all-clients name client-sock
				    (request-src req) req))
	 
	 (else
	  (format logging "Unrecognized action requested: ~A. Letting sender know.~%" (request-action req))
	  (if (not
	       (send-request (make-request 'server
					   (request-dst req)
					   'receive-msg
					   (format #f "Unrecognized action: ~A"
						   (request-action req)))
			     port-to-client))
	      (remove-client name)) ))))
  ;; if other data ready, handle them now    
  (if (and (not (port-closed? port-from-client))
	   (char-ready? port-from-client))
      (client-request-handler)) )

(define (setup-client-request-handler name client-sock)
  ;;;Handle messages from the client.
  ;
  ;Only handles "send-msg" and "logout" messages.
  ;
  
  (define (client-request-handler)
    (let* ((port-from-client (socket-input client-sock))
	   (port-to-client (socket-output client-sock))
	   (req (get-request port-from-client)))
      (eventless-client-request-handler name client-sock port-from-client port-to-client req)))
  ;; Set up the handler
  (when-port-readable (socket-input client-sock) client-request-handler))


