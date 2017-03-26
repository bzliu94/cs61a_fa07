; 1.

; according to the google mapreduce article, in order to distribute reduce phase across multiple machines, we ought to have more unique intermediate keys that we then manually combine

; we are trading one bottleneck (reduce un-parallelized) for another (more time for pre-processing un-parallelized)

; not all bottlenecks are equal

; we group by play

(define result
  (lmapreduce
   (lambda (input-key-value-pair)
     (let ((play (car input-key-value-pair)))
       (list (make-kv-pair play 1))))
   +
   0
   "/shakespeare-short"))

(define next-result (stream-map (lambda (x) (cdr x)) result))

(stream-accumulate (lambda (x y) (+ x y)) 0 next-result)

; caesar - 2163
; hamlet - 3106
; macbeth - 1907

; (kv-value (stream-car result))

; 2a.

; we're doing this problem locally; 
; we are using a smaller corpus 
; so that the methods finish quickly enough

(define result
  (lmapreduce
   (lambda (input-key-value-pair)
     (let ((play (car input-key-value-pair))
           (words (cdr input-key-value-pair)))
       (let ((pairs
	      (map (lambda (word)
		     (make-kv-pair word 1))
		   words)))
	 ; (begin
	   ; (display pairs)
	   pairs)))
     +
     0
     "/beatles-songs"))

(define (force-whole-stream s)
  (force-whole-stream-helper s))

(define (force-whole-stream-helper s)
  (if (equal? s the-empty-stream)
      nil
      (cons (stream-car s) (force-whole-stream-helper (stream-cdr s)))))

(define (reverse l)
  (cond ((null? l) nil)
	(else
	 (let ((prev-element (car l))
	       (rest-of-list (cdr l))
	       (partial-result nil))
	   (reverse-helper partial-result rest-of-list prev-element)))))

(define (reverse-helper partial-result l prev-element)
  (cond ((null? l) (cons prev-element partial-result))
	(else
	 (let ((next-prev-element (car l))
	       (next-rest-of-list (cdr l))
	       (next-partial-result (cons prev-element partial-result)))
	   (reverse-helper next-partial-result next-rest-of-list next-prev-element)))))

; 2b.

(define result-sorted (sort (force-whole-stream result) (lambda (x y) (< (cdr x) (cdr y)))))

(define result-sorted-reversed (reverse result-sorted))

(car (car result-sorted-reversed))

; 'you' for beatles-songs

; 2c.

(define (words-used-once word-stream)
  (stream-filter (lambda (x) (equal? (cdr x) 1)) word-stream))

(words-used-once result)

; 3a.

; we are to make our own match? predicate

; specified words must match exactly

; * matches zero or more words

; matched text does not have to include all words from source

(define (display-line x)
  (newline)
  (display x))

(define (match? pattern-string-list word-list)
  (match?-helper pattern-string-list word-list))

(define (match?-helper pattern-string-list word-list)
  (cond 
        ; no pattern left; automatically succeed
        ((equal? pattern-string-list nil) #t)
        ; no candidate words left; could succeed
	((equal? word-list nil)
	 (if (equal? (car pattern-string-list) '*)
	     (match?-helper (cdr pattern-string-list) word-list)
	     #f))
        ; have pattern and candidate words left
	(else (let ((pattern-string (car pattern-string-list))
		    (curr-word (car word-list))
		    (next-pattern-string-list (cdr pattern-string-list))
		    (next-word-list (cdr word-list)))
		; (begin (display-line pattern-string) (display-line curr-word) (newline)
		(if (equal? pattern-string '*)
                    ; consume pattern or don't; avoid infinite loop
		    (or (match?-helper next-pattern-string-list next-word-list)
			(match?-helper pattern-string-list next-word-list)
			(match?-helper next-pattern-string-list word-list))
                    ; consume pattern
		    (or 
		     (and (equal? pattern-string curr-word)
			  (match?-helper next-pattern-string-list next-word-list))
		     (match?-helper pattern-string-list next-word-list)))))))

(define (match-mr pattern-string-list corpus-directory-name)
  (lmapreduce
   (lambda (input-key-value-pair)
     (let ((play (car input-key-value-pair))
           (curr-sentence (cdr input-key-value-pair)))
       (if (match? pattern-string-list curr-sentence)
	   (list input-key-value-pair)
	   nil)))
   (lambda (x y) (if (equal? x nil)
		     y
		     x))
     nil
     corpus-directory-name))

(define (get-pattern-matched-sentence-stream pattern-string-list corpus-directory-name)
  (match-mr pattern-string-list corpus-directory-name))

(get-pattern-matched-sentence-stream '(* hard * night *) "/beatles-songs")

; 3b.

; we were not able to use full shakespeare corpus, and even short shakespeare corpus was too slow for our personal machine (i.e., we don't have access to a cluster); we use beatles song lyrics, instead

(define r1 (get-pattern-matched-sentence-stream '(need is love) "/beatles-songs"))

(define r2 (get-pattern-matched-sentence-stream '(helter skelter) "/beatles-songs"))

(define r3 (get-pattern-matched-sentence-stream '(* * lucy * sky * diamonds) "/beatles-songs"))

(force-whole-stream r1)

; ((magical-mystery-tour all you need is love) (yellow-submarine all you need is love))

(force-whole-stream r2)

; ((beatles-white-album helter skelter))

(force-whole-stream r3)

; ((sgt-peppers-lonely-hearts-club-band lucy in the sky with diamonds))


