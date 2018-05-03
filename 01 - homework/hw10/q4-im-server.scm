; 4.

; (this file replaces the server)

(load "im-server.scm")

; (have refuse-aware send)

(define (eventless-client-request-handler name client-sock port-from-client port-to-client req)
  (if (not req)
      (remove-client name)
      (begin	    
	(format logging "Received request: ~S~%" req)
	(cond
	 ((equal? 'send-msg (request-action req))
	  (let ((client-src (request-src req))
		(client-dst (request-dst req)))
	    (let ((curr-refuse-set (get 'refuse-set client-dst)))
	      (if (and
		   (not (eq? curr-refuse-set #f))
		   (not (eq? (memq client-src curr-refuse-set) #f)))
		  ; relevant refuse exists
		  (begin
		    (format logging "A send-msg message from client ~A to client ~A is refused.~%" client-src client-dst)
		    (let ((port-to-src (find-port-to-client (request-src req))))
		      (send-request (make-request (request-dst req)
						  (request-src req)
						  'handle-refuse-instance
						  nil)
				    port-to-src)))
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
			      (remove-client name)) ))) ))))
	      
	      ((equal? 'logout (request-action req))
	       (remove-client name))

	      ((equal? 'do-refuse (request-action req))
	       (do-refuse name (request-dst req)))
	      
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

(define (do-refuse client-src client-dst)
  (format logging "Client ~A wishes to refuse ~A.~%" client-src client-dst)
  (let ((curr-refuse-set (get 'refuse-set client-src)))
    (if (eq? (member client-dst (get-clients-list)) #f)
	(format logging "Proposed refusee client ~A does not exist.~%" client-dst)
	(if (eq? curr-refuse-set #f)
	    (begin
	      (format logging "Refuse set for client ~A is initialized to have client ~A.~%" client-src client-dst)
	      (put 'refuse-set client-src (list client-dst)))
	    (if (eq? (memq client-dst curr-refuse-set) #f)
		(let ((next-refuse-set (cons client-dst curr-refuse-set)))
		  (format logging "Refuse set for client ~A has client ~A added.~%" client-src client-dst)
		  (put 'refuse-set client-src next-refuse-set))
		(let ((next-refuse-set curr-refuse-set))
		  (format logging "Refuse set for client ~A already had client ~A.~%" client-src client-dst)
		  (put 'refuse-set client-src next-refuse-set)))))))

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


