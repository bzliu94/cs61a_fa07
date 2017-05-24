; note that we are not using true objects; 
; we are not using message passing; 
; inheritance does not occur

(define items '(the rain in spain stays mainly on the plain))

(define (regroup primary-pattern)
  (lambda (items)
    #t))

; (1 2 3 4 5 7 8 9)

; ((1 2) (3 4) (5 6) (7 8))
(define pairup-primary-pattern '((1 2) (3 4) ...))
(define pairup (regroup pairup-primary-pattern))

; ((1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 8) (8 9))
(define overlap-primary-pattern '((1 2) (2 3) ...))
(define overlap (regroup overlap-primary-pattern))

; ((1 2 3 4 5 6 7 8 9)
; (2 3 4 5 6 7 8 9)
; (3 4 5 6 7 8 9)
; (4 5 6 7 8 9)
; (5 6 7 8 9)
; (6 7 8 9)
; (7 8 9)
; (8 9)
; (9))
(define tails-primary-pattern '((1 2 ...) (2 3 ...) ...))
(define tails (regroup tails-primary-pattern))

; (see above)
(define tails2-primary-pattern '((1 ...) ...))
(define tails2 (regroup tails2-primary-pattern))

; (2 1 4 3 6 5 8 7)
(define swap-primary-pattern '(2 1 4 3 ...))
(define swap (regroup swap-primary-pattern))

; ((1 3 5 7 9) (2 4 5 6 8))
(define split-primary-pattern '((1 3 ...) (2 4 ...)))
(define split (regroup split-primary-pattern))

(define (flatten curr-list)
  (cond ((not (list? curr-list)) (list curr-list))
	((eq? (length curr-list) 0) nil)
	((eq? (length curr-list) 1) (flatten (car curr-list)))
	(else (append (flatten (car curr-list))
		      (flatten (cdr curr-list))))))

; (display (flatten 4))
; (newline)

; (display (flatten '(1 (2 3 (4)) 5)))
; (newline)

(define (listEndsInEllipsis curr-list)
  (cond ((eq? (length curr-list) 0) #f)
	((eq? (list-ref curr-list (- (length curr-list) 1)) '...) #t)
	(else #f)))

; (display (listEndsInEllipsis '(1 2 3 ...)))
; (newline)

(define (getFirstScalar curr-list)
  (let ((candidate-values (filter (lambda (x) (integer? x)) curr-list)))
    (if (eq? (length candidate-values) 0)
	nil
	(car candidate-values))))

; (display (getFirstScalar (flatten '(1 (2 3 (4 ...)) 5 ...))))
; (newline)

(define (join str-list separator)
  (reduce (lambda (a b) (string-append a separator b)) str-list))

; Pattern class

(define (make-pattern parent-pattern)
  (list 'pattern parent-pattern nil nil nil))

(define (getParentPattern pattern)
  (cadr pattern))

(define (getSubpatterns pattern)
  (caddr pattern))

(define (setSubpatterns pattern subpatterns)
  (set-car! (cddr pattern) subpatterns))

(define (toLOPForm listified-pattern)
  (let ((flattened-listified-pattern (flatten listified-pattern)))
    (if (eq? (length flattened-listified-pattern) 0)
	(list nil nil)
	(let ((top-leftmost-offset (car flattened-listified-pattern)))
	  (let ((result (toLOPFormHelper
			 listified-pattern
			 top-leftmost-offset
			 nil)))
	    (let ((next-result (list result top-leftmost-offset)))
	      next-result))))))

(define (toLOPFormHelper listified-pattern base-index parent-pattern)
  (cond ((list? listified-pattern)
	 (let ((ends-in-ellipsis (listEndsInEllipsis listified-pattern)))
	   (let ((pattern (make-list-pattern parent-pattern nil ends-in-ellipsis))
		 (subpatterns nil)
		 (prev-base-index nil)
		 (curr-base-index base-index)
		 (num-indices (- (length listified-pattern)
				 (if ends-in-ellipsis 1 0))))
	     (begin
	       (dotimes (i num-indices)
			(let ((curr-listified-pattern
			       (list-ref listified-pattern i)))
			  (if (not (eq? curr-listified-pattern '...))
			      (let ((curr-pattern1 (toLOPFormHelper curr-listified-pattern curr-base-index pattern)))
				(begin
				  (set! subpatterns (append subpatterns (list curr-pattern1)))
				  (if (not (eq? i (- num-indices 1)))
				      (begin
					(set! prev-base-index curr-base-index)
					(set! curr-base-index (car (flatten (list-ref listified-pattern (+ i 1)))))
					(let ((curr-offset (- curr-base-index prev-base-index)))
					  (let ((curr-pattern2 (make-offset-pattern pattern curr-offset)))
					    (set! subpatterns (append subpatterns (list curr-pattern2))))))))))))
	       (setSubpatterns pattern subpatterns)
	       pattern))))
	((integer? listified-pattern)
	 (let ((pattern (make-impulse-pattern parent-pattern)))
	   pattern))))

(define (isListPattern pattern)
  (eq? (car pattern) 'list-pattern))

(define (isOffsetPattern pattern)
  (eq? (car pattern) 'offset-pattern))

(define (isImpulsePattern pattern)
  (eq? (car pattern) 'impulse-pattern))

(define (pattern-toString pattern)
  (cond ((isListPattern pattern) (string-append "(LP-{" (if (pattern-isExpandable pattern) "T" "F") "} " (join (map (lambda (x) (pattern-toString x)) (getSubpatterns pattern)) " ") ")"))
	((isOffsetPattern pattern) (number->string (pattern-getSize pattern)))
	((isImpulsePattern pattern) "I")))

(define (make-list-pattern parent-pattern subpatterns is-expandable)
  (list 'list-pattern parent-pattern subpatterns nil is-expandable))

(define (make-offset-pattern parent-pattern size)
  (list 'offset-pattern parent-pattern nil size nil))

(define (make-impulse-pattern parent-pattern)
  (list 'impulse-pattern parent-pattern nil nil nil))

; ListPattern class

(define (pattern-isExpandable list-pattern)
  (list-ref list-pattern 4))

(define (toIsomorphicCollapsedBadges pattern)
  (cond ((isListPattern pattern)
	 (let ((btioviettbd (make-hash-table))
	       (ovtobd (make-hash-table)))
	   (exploreTICB pattern btioviettbd ovtobd)))
	((isOffsetPattern pattern) nil)
	((isImpulsePattern pattern) nil)))

(define (exploreTICB pattern btioviettbd ovtobd)
  (cond ((isListPattern pattern)
	 (let ((subpatterns (getSubpatterns pattern))
	       (subbadges nil))
	   (begin
	     (for-each
	      (lambda (x)
		(let ((subbadge-list (exploreTICB x btioviettbd ovtobd)))
		  (set! subbadges (append subbadges subbadge-list))))
	      subpatterns)
	     (let ((badges (postVisitTICB pattern btioviettbd ovtobd subbadges)))
	       badges))))
	((isOffsetPattern pattern)
	 (let ((badges (postVisitTICB pattern btioviettbd ovtobd nil)))
	   badges))
	((isImpulsePattern pattern)
	 (let ((badges (postVisitTICB pattern btioviettbd ovtobd nil)))
	   badges))))

(define (postVisitTICB pattern btioviettbd ovtobd subbadges)
  (cond ((isListPattern pattern)
	 (let ((result (headWildcardSRSPExceptWhenSizeLEQTwo (append (list nil) subbadges))))
	   (let ((result-with-standard-head (car result))
		 (result-with-special-head (cadr result)))
	     (let ((next-subbadges nil)
		   (internal-offset-value nil))
	       (begin;
		 (if (eq? (pattern-isExpandable pattern) #f)
		     (set! next-subbadges subbadges)
		     (begin
		       (let ((internal-offset-badge (car result-with-standard-head)))
			 (if (eq? internal-offset-badge nil)
			     (set! internal-offset-value 1)
			     (set! internal-offset-value (badge-getSize internal-offset-badge)))
			 (let ((offsetless-pattern (subseq result-with-special-head 1 (length result-with-special-head))))
			   (set! next-subbadges offsetless-pattern)))))
		 (let ((badge-tuple next-subbadges))
		   (let ((btioviet (list badge-tuple internal-offset-value (pattern-isExpandable pattern)))
			 (badge nil))
		     (begin
		       (let ((result (hash-table-get btioviettbd btioviet nil)))
			 (if (eq? result nil)
			     (begin
			       (set! badge (make-list-badge next-subbadges internal-offset-value (pattern-isExpandable pattern)))
			       (hash-table-put! btioviettbd btioviet badge))
			     (set! badge result)))
		       (list badge)))))))))
	((isOffsetPattern pattern)
	 (let ((offset-value (pattern-getSize pattern))
	       (badge nil))
	   (begin
	     (let ((result (hash-table-get ovtobd offset-value nil)))
	       (if (eq? result nil)
		   (begin
		     (set! badge (make-offset-badge offset-value))
		     (hash-table-put! ovtobd offset-value badge))
		   (set! badge result)))
	     (list badge))))
	((isImpulsePattern pattern)
	 (let ((badge IMPULSE-BADGE))
	   (list badge)))))

; OffsetPattern class

(define (pattern-getSize offset-pattern)
  (cadddr offset-pattern))

; ImpulsePattern class

; Badge class

(define (isListBadge badge)
  (eq? (car badge) 'list-badge))

(define (isOffsetBadge badge)
  (eq? (car badge) 'offset-badge))

(define (isImpulseBadge badge)
  (eq? (car badge) 'impulse-badge))

(define (make-list-badge subbadge-repeating-unit-list internal-offset-value is-expandable)
  (list 'list-badge subbadge-repeating-unit-list internal-offset-value is-expandable nil))

(define (make-offset-badge size)
  (list 'offset-badge nil nil nil size))

(define (make-impulse-badge)
  (list 'impulse-badge nil nil nil nil))

(define (badge-toString badge)
  (cond ((isListBadge badge) (string-append "(LB-{" (getInternalOffsetValueString badge) "}-{" (if (badge-isExpandable badge) "T" "F") "} " (join (map (lambda (x) (badge-toString x)) (getSubbadgeRUL badge)) " ") ")"))
	((isOffsetBadge badge) (number->string (badge-getSize badge)))
	((isImpulseBadge badge) "I")))

(define IMPULSE-BADGE (make-impulse-badge))

; ListBadge class

(define (getSubbadgeRUL list-badge)
  (cadr list-badge))

(define (getInternalOffsetValue list-badge)
  (caddr list-badge))

; internal offset value can be nil

(define (getInternalOffsetValueString list-badge)
  (let ((internal-offset-value (getInternalOffsetValue list-badge)))
    (if (eq? internal-offset-value nil)
	"None"
	(number->string internal-offset-value))))

(define (badge-isExpandable list-badge)
  (cadddr list-badge))

; OffsetBadge class

(define (badge-getSize offset-badge)
  (list-ref offset-badge 4))

; ImpulseBadge class

(define (toMaskIndexList badge max-allowed-index offset)
  (cond ((isListBadge badge)
	 (let ((curr-offset offset)
	       (subbadge-rul-vector (list->vector (getSubbadgeRUL badge)))
	       (internal-offset-value (getInternalOffsetValue badge))
	       (is-expandable (badge-isExpandable badge))
	       (num-instances-seen 0)
	       (curr-result nil)
	       (stop-early #f)
	       (result (list nil #t)))
	   (begin
	     (while
	      (and #t (eq? stop-early #f))
	      (let ((curr-instance nil)
		    (i 0))
		(begin
		  (while (and
			  (< i (vector-length subbadge-rul-vector))
			  (eq? stop-early #f))
			 (let ((element-badge (vector-ref subbadge-rul-vector i)))
			   (let ((next-curr-result (toMaskIndexList
						    element-badge
						    max-allowed-index
						    curr-offset)))
			     (let ((mask (car next-curr-result))
				   (is-successful (cadr next-curr-result)))
			       (if (eq? is-successful #t)
				   (if (eq? (isOffsetBadge element-badge) #t)
				       (set! curr-offset
					     (+ curr-offset (badge-getSize element-badge)))
				       (if (not (eq? mask nil))
					   (set! curr-instance (append curr-instance (list mask)))))
				   (begin
				     (set! stop-early #t)
				     (if (>= num-instances-seen 1)
					 (set! result (list curr-result #t))
					 (set! result (list nil #f))))))))
			 (set! i (+ i 1)))
		  (if (not (eq? stop-early #t))
		      (begin
			(set! curr-result (append curr-result curr-instance))
			(set! num-instances-seen (+ num-instances-seen 1))
			(if (eq? is-expandable #t)
			    (set! curr-offset (+ curr-offset internal-offset-value))
			    (begin
			      (set! result (list curr-result #t))
			      (set! stop-early #t))))))))
	     result)))
	((isOffsetBadge badge)
	 (list nil #t))
	((isImpulseBadge badge)
	 (if (or (> offset max-allowed-index) (< offset 1))
	     (list nil #f)
	     (list offset #t)))))
      
(define (applyIndexMaskToItemList index-mask item-list)
  (applyIndexMaskToItemListHelper index-mask (list->vector item-list)))

(define (applyIndexMaskToItemListHelper index-mask item-vector)
  (cond ((integer? index-mask) (vector-ref item-vector (- index-mask 1)))
	((list? index-mask)
	 (if (eq? (length index-mask) 0)
	     nil
	     (let ((curr-mask (car index-mask))
		   (rest-of-mask (cdr index-mask)))
	       (let ((result1 (applyIndexMaskToItemListHelper curr-mask item-vector))
		     (result2 (applyIndexMaskToItemListHelper rest-of-mask item-vector)))
		 (cons result1 result2)))))))

(define pattern-list (list pairup-primary-pattern overlap-primary-pattern tails-primary-pattern tails2-primary-pattern swap-primary-pattern split-primary-pattern))

; SRSP-related methods

; returns a list

(define (KMPFailureFunction pattern-list)
  (let ((i 1)
	(j 0)
	(m (length pattern-list))
	(pattern-vector (list->vector pattern-list)))
    (let ((f (make-vector m 0)))
      (begin
	(while
	 (< i m)
	 (cond ((equal? (vector-ref pattern-vector j)
			(vector-ref pattern-vector i))
		(begin
		  (vector-set! f i (+ j 1))
		  (set! i (+ i 1))
		  (set! j (+ j 1))))
	       ((> j 0)
		(set! j (vector-ref f (- j 1))))
	       (else
		(begin
		  (vector-set! f i 0)
		  (set! i (+ i 1))))))
	(vector->list f)))))

; returns a symbol list

(define (SRSP pattern-list)
  (if (eq? (length pattern-list) 0)
      nil
      (let ((m (length pattern-list))
	    (f (KMPFailureFunction pattern-list)))
	(let ((proper-suffix-size (list-ref f (- m 1))))
	  (let ((left-piece-size (- m proper-suffix-size)))
	    (if (eq? (modulo m left-piece-size) 0)
		(subseq pattern-list 0 left-piece-size)
		pattern-list))))))

(define (strToSymbolList str)
  (map string->symbol (map string (string->list str))))

(define (symbolListToStr symbol-list)
  (let ((str-list (map (lambda (x) (symbol->string x)) symbol-list)))
    (reduce (lambda (a b) (string-append a b)) str-list)))

; returns an integer

; T and P are symbol lists
(define (KMPMatch T P)
  (let ((n (length T))
	(m (length P))
	(f (KMPFailureFunction P))
	(i 0)
	(j 0)
	(T-vector (list->vector T))
	(P-vector (list->vector P))
	(stop-early #f))
    (begin
      (while
       (and (< i n) (eq? stop-early #f))
       (begin
	 (cond ((equal? (vector-ref P-vector j)
			(vector-ref T-vector i))
		(begin
		  (if (eq? j (- m 1))
		      (set! stop-early #t)
		      (begin
			(set! i (+ i 1))
			(set! j (+ j 1))))))
	       ((> j 0) (set! j (list-ref f (- j 1))))
	       (else (set! i (+ i 1))))))
      (if (eq? stop-early #t)
	  (+ (- i m) 1)
	  (error "no substring of T matching P")))))

; returns a symbol list

(define (KMPFailureFunctionWithHeadWildcard pattern-list do-head-wildcard)
  (let ((i 1)
	(j 0)
	(m (length pattern-list))
	(pattern-vector (list->vector pattern-list)))
    (let ((f (make-vector m 0)))
      (begin
	(while
	 (< i m)
	 (cond ((or
		 (equal? (vector-ref pattern-vector j)
			 (vector-ref pattern-vector i))
		 (and (eq? j 0)
		      (eq? do-head-wildcard #t)))
		(begin
		  (vector-set! f i (+ j 1))
		  (set! i (+ i 1))
		  (set! j (+ j 1))))
	       ((> j 0)
		(set! j (vector-ref f (- j 1))))
	       (else
		(vector-set! f i 0)
		(set! i (+ i 1)))))
	(vector->list f)))))

; returns a list of two symbol lists

(define (headWildcardSRSP pattern-list)
  (if (eq? (length pattern-list) 0)
      nil
      (let ((m (length pattern-list))
	    (f (KMPFailureFunctionWithHeadWildcard pattern-list #t)))
	(let ((proper-suffix-size (list-ref f (- m 1))))
	  (let ((left-piece-size (- m proper-suffix-size)))
	    (if (eq? (modulo m left-piece-size) 0)
		(let ((result-with-standard-head nil)
		      (result-with-special-head nil))
		  (begin
		    (if (eq? m left-piece-size)
			(begin
			  (set! result-with-special-head pattern-list)
			  (set! result-with-standard-head pattern-list))
			(begin
			  (set! result-with-special-head (subseq pattern-list 0 left-piece-size))
			  (set! result-with-standard-head (subseq pattern-list left-piece-size (* left-piece-size 2)))))
		    (let ((result (list result-with-standard-head result-with-special-head)))
		      result)))
		(let ((result (list pattern-list pattern-list)))
		  result)))))))

; returns a list of two symbol lists

(define (headWildcardSRSPExceptWhenSizeLEQTwo pattern-list)
  (let ((n (length pattern-list)))
    (if (<= n 2)
	(let ((result (list pattern-list pattern-list)))
	  result)
	(if (>= n 3)
	    (headWildcardSRSP pattern-list)))))

#|

(display (SRSP (strToSymbolList "abcabcabc")))
(newline)
(display (eq? (KMPMatch (strToSymbolList "abacaabaccabacabaabb") (strToStrList "abacab")) 10))
(newline)
(display (headWildcardSRSP (strToSymbolList "abcabc")))
(newline)
(display (headWildcardSRSP (strToSymbolList "cbcabc")))
(newline)
(display (headWildcardSRSP (strToSymbolList "abcabcd")))
(newline)
(display (headWildcardSRSP (strToSymbolList "ab")))
(newline)
(display (headWildcardSRSPExceptWhenSizeLEQTwo (strToSymbolList "ab")))
(newline)
(display (headWildcardSRSPExceptWhenSizeLEQTwo (strToSymbolList "a")))
(newline)
(display (SRSP '((1) (2) (1) (2))))
(newline)

|#

(for-each
 (lambda (x)
   (begin
     (let ((listified-pattern x))
       (let ((result (toLOPForm listified-pattern)))
	 (let ((root-pattern (car result))
	       (leftmost-offset (cadr result)))
	   (begin
	     ; (display (pattern-toString root-pattern))
	     ; (newline)
	     (let ((badges (toIsomorphicCollapsedBadges root-pattern)))
	       (let ((root-badge (car badges))
		     (max-allowed-index (length items)))
		 (let ((next-result (toMaskIndexList root-badge max-allowed-index leftmost-offset)))
		   (begin
		     (display (string-append (number->string leftmost-offset) " "))
		     (display (badge-toString root-badge))
		     (newline)
		     (let ((index-mask (car next-result))
			   (is-successful (cadr next-result)))
		       (begin
			 (display index-mask)
			 (newline)
			 (let ((words-regrouped (applyIndexMaskToItemList index-mask items)))
			   (begin
			     (display words-regrouped)
			     (newline)
			     (newline)))))))))))))))
 pattern-list)

