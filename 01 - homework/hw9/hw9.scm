; 1.

; ex. 3.16

; see "ex. 3.16 a.png", "ex. 3.16 b.png", 
; "ex. 3.16 c.png", "ex. 3.16 d.png"

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; the point is that we could have aliasing

; a) 3 -> 3

(define l1 (cons 1 (cons 2 (cons 3 nil))))
(display (count-pairs l1))

; b) 3 -> 4

(define l2 (cons nil (cons 2 (cons 3 nil))))
(set-car! l2 (cddr l2))
(display (count-pairs l2))

; c) 3-> 7

(define l3 (cons nil (cons nil (cons 3 nil))))
(set-car! l3 (cdr l3))
(set-car! (cdr l3) (cddr l3))
(display (count-pairs l3))

; d) 3 -> infinite loop

(define l4 (cons nil (cons 2 (cons 3 nil))))
(set-car! l4 l4)
; (display (count-pairs l4))

(display "\n")

; 2.

; ex. 3.17

; we're not using put/get because they use equal? instead of eq? for keys

; we're not using local hash table as mentioned in section 3.3.3 
; because their implementation uses a list anyway

; we don't use list directly because we feel this is not the best 
; level of abstraction for describing the structure that we need

; we use a set (as mentioned in section 2.3.3) that 
; happens to be implemented using a list 
; and that has been modified to use eq? instead of equal?

; also, a set (which is a higher-level concept) 
; can in the future be modified to use hash table 
; that uses hash code and thereby has time O(1) for 
; mutator and getter/setter methods

; this question is essentially about DFS

; also, we arbitrarily decide to handle having directed cycles

(define (make-pointer-set items)
  (let ((curr-set nil))
    (make-pointer-set-helper curr-set items)))

(define (make-pointer-set-helper curr-set items)
  (cond ((empty? items) curr-set)
	(else (let ((curr-item (car items))
		    (remaining-items (cdr items)))
		(make-pointer-set-helper
		 (adjoin-pointer-set curr-item curr-set)
		 remaining-items)))))

(define (adjoin-pointer-set x set)
  (if (element-of-pointer-set? x set)
      set
      (cons x set)))

(define (element-of-pointer-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-pointer-set? x (cdr set)))))

(define (count-pairs-correct x)
  (define pointer-set (make-pointer-set nil))
  (define (count-pairs-correct-helper curr-x)
    (if (not (pair? curr-x))
	0
	(let ((seen-pair-before (element-of-pointer-set? curr-x pointer-set)))
	  (if seen-pair-before
	      0
	      (begin
		(set! pointer-set (adjoin-pointer-set curr-x pointer-set))
		(+ (count-pairs-correct-helper (car curr-x))
		   (count-pairs-correct-helper (cdr curr-x))
		   1))))))
  (count-pairs-correct-helper x))

(display (count-pairs-correct l1))
(display (count-pairs-correct l2))
(display (count-pairs-correct l3))

; if we are not careful, the following list
; looks like it has three pairs and not four

(define l5 (cons nil (cons 2 (cons 3 nil))))
(set-car! l5 (cons 2 (cddr l5)))
(display (count-pairs-correct l5))

(display "\n")

; 3.

; ex. 3.21

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(display q1)
(display "\n")
; ((a) a)
(insert-queue! q1 'b)
(display q1)
(display "\n")
; ((a b) b)
(delete-queue! q1)
(display q1)
(display "\n")
; ((b) b)
(delete-queue! q1)
(display q1)
(display "\n")
; (() b)

; the reason we are getting the outputs that we are 
; is that the queue value has an arbitrary structure; 
; the car of the queue is an ordered list 
; and the cdr of the queue is a pointer to last pair, 
; even if there are no items in our queue s.t. it can 
; be outdated; to get the right output, 
; we will have to have print-queue take the car of 
; the queue value, assuming that we want earliest-added 
; items at left and latest-added items at right

(define (print-queue q)
  (let ((items (car q)))
    (display items)))

(print-queue q1)
(display "\n")

; 4.

; ex. 3.25

; we handle arbitrary number of keys for each item
; by combining the keys s.t. we end up with only one key

; by having local table, we have full control
; over our implementation; we provide a special symbol
; that we mix in even if we have e.g. only one provided key
; for an item s.t. we avoid undesired collisions 

; we assume l is non-empty; 
; we note that this algorithm
; is not as fast as it could be
(define (bl l)
  (if (< (length l) 1)
      (error "given list needs to be non-empty")
      (bl-helper nil l)))
(define (bl-helper created-list l-remaining)
  (cond ((eq? (length l-remaining) 1)
	 created-list)
	(else
	 (let ((curr-item (car l-remaining))
	       (next-l-remaining (cdr l-remaining)))
	   (bl-helper (append created-list (list curr-item))
		      next-l-remaining)))))

; we assume l is non-empty; 
; we note that this algorithm 
; is not as fast as it could be
(define (last l)
  (if (< (length l) 1)
      (error "given list needs to be non-empty")
      (last-helper l)))
(define (last-helper curr-l)
  (cond ((eq? (length curr-l) 1)
	 (car curr-l))
	(else (last-helper (cdr curr-l)))))

(define (make-variable-arity-key-table)
  (define local-table (list '*table*))
  (define key-tag '*key*)
  (define (assoc key records)
    (cond ((null? records) false)
	  ((equal? key (caar records)) (car records))
	  (else (assoc key (cdr records)))))
  (define (lookup-single-key key)
    (let ((record (assoc key (cdr local-table))))
      (if record
	  (cdr record)
	  false)))
  (define (insert-single-key! key value)
    (let ((record (assoc key (cdr local-table))))
      (if record
	  (set-cdr! record value)
	  (set-cdr! local-table
		    (cons (cons key value) (cdr local-table)))))
    'ok)
  ; args is (key_1 key_2 ... key_n); 
  ; we assume we have at least one component key
  (define (lookup . args)
    (let ((keys args))
      (let ((next-key (cons key-tag keys)))
	(lookup-single-key next-key))))
  ; args is (key_1 key_2 ... key_n value); 
  ; we assume we have at least one component key
  (define (insert! . args)
    (let ((keys (bl args))
	  (value (last args)))
      (let ((next-key (cons key-tag keys)))
	(insert-single-key! next-key value))))
  (define (dispatch m)
    (cond ((eq? m 'lookup-proc) lookup)
	  ((eq? m 'insert-proc!) insert!)
	  (else (error "Unknown operation -- TABLE" m))))
  dispatch)

(define curr-table (make-variable-arity-key-table))
(define curr-get (curr-table 'lookup-proc))
(define curr-put (curr-table 'insert-proc!))

(curr-put 'math 1)
(display (curr-get 'math))

(curr-put 'math '+ 2)
(display (curr-get 'math '+))

(curr-put 'math '+ '+ 3)
(display (curr-get 'math '+ '+))

(display (curr-get 'math))

(display (curr-get 'letters))

(display "\n")

; 5.

; ex. 3.27

; use table for memoization

; we show how this reduces work for finding nth fibonacci number fib(n) 
; from exponential in n to linear in n

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)
(define (make-table)
  (list '*table*))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(display (fib 3))
(display (memo-fib 3))

(display "\n")

; before we attempt to answer the questions, we first
; point out that memo-fib is defined not as a procedure, 
; but as a standard value, which means what it defines to 
; is evaluated at define time, as opposed to what happens
; with a procedure (i.e. body would then not be evaluated
; until a call to the procedure)

; a) draw environment diagram for (memo-fib 3)

; see "ex. 3.27 a.png"

; as per assignment instructions, we technically
; don't need to draw the environment diagram;
; also, we treat lookup and insert! as primitive; 
; assuming those procedures work in constant time, 
; we then want to use trace to show
; number of times memo-fib is invoked

; b) why is time O(n) and not O(2 ^ n)?

; consider dependency graph

; two issues contribute to this time: 
; i) there are number of nodes linear in n
; ii) in this directed graph, we have no directed cycles and so
;     topological ordering is possible and dictates the order 
;     in which we determine node values for memoization

; c) would setting memo-fib to be (memoize fib) work?

; no, because we would only memoize once per unique query, 
; which is far too late for us to see a speed-up using, 
; unless we have a huge number of queries, in which case 
; we could still be far slower than we could be

; vector exercises

; don't use list as intermediate value; don't convert vectors to lists

; vector is an array, in contrast to list

; 1.

; vector-append

(define (vector-append v1 v2)
  (define (loop v_in i_in v_out i_out)
    (if (< i_in 0)
	v_out
	(begin
	  (vector-set! v_out i_out (vector-ref v_in i_in))
	  (loop v_in (- i_in 1) v_out (- i_out 1)))))
  (let ((length1 (vector-length v1))
	(length2 (vector-length v2)))
    (let ((target-length (+ length1 length2)))
      (let ((result (make-vector target-length)))
	(begin
	  (loop v1 (- length1 1) result (- length1 1))
	  (loop v2 (- length2 1) result (- (+ length1 length2) 1))
	  result)))))

; 2.

; vector-filter

(define (vector-map fn vec)
  (define (loop newvec n)
    (if (< n 0)
	newvec
	(begin (vector-set! newvec n (fn (vector-ref vec n)))
	       (loop newvec (- n 1)))))
  (loop (make-vector (vector-length vec)) (- (vector-length vec) 1)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (vector-filter pred vec)
  (define (loop1 i)
    (if (< i 0)
	0
	(let ((curr-element (vector-ref vec i)))
	  (let ((do-keep (pred curr-element)))
	    (if do-keep
		(+ 1 (loop1 (- i 1)))
		(loop1 (- i 1)))))))
  (define (loop2 output-vec i-in i-out)
    (if (< i-in 0)
	output-vec
	(let ((curr-element (vector-ref vec i-in)))
	  (let ((do-keep (pred curr-element)))
	    (if do-keep
		(begin
		  (vector-set! output-vec i-out curr-element)
		  (loop2 output-vec (- i-in 1) (- i-out 1)))
		(loop2 output-vec (- i-in 1) i-out))))))
  (let ((num-keep-elements (loop1 (- (vector-length vec) 1))))
    (let ((result (make-vector num-keep-elements)))
      (begin
	(loop2 result (- (vector-length vec) 1)
	       (- num-keep-elements 1))
	result))))

; this version is just as fast

(define (vector-filter-old pred vec)
  (list->vector (filter pred (vector->list vec))))

; 3.

; a) bubble-sort! for vector

(define (bubble-sort! vec)
  (define overall-length (vector-length vec))
  (define (loop1 curr-i max-left-el-i)
    (if (> curr-i max-left-el-i)
	vec
	(let ((left-element (vector-ref vec curr-i))
	      (right-element (vector-ref vec (+ curr-i 1))))
	  (begin
	    (if (> left-element right-element)
		(begin
		  (vector-set! vec curr-i right-element)
		  (vector-set! vec (+ curr-i 1) left-element))
		nil)
	    (loop1 (+ curr-i 1) max-left-el-i)))))
  (define (loop2 num-elements-left)
    (if (<= num-elements-left 1)
	vec
	(begin
	  (loop1 0 (- num-elements-left 2))
	  (loop2 (- num-elements-left 1)))))
  (let ((overall-length (vector-length vec)))
    (let ((curr-length overall-length))
      (loop2 curr-length))))

(define curr-vec #(4 3 1 5 2))

(display curr-vec)
(display "\n")

(bubble-sort! curr-vec)
(display curr-vec)
(display "\n")

; b) prove that the algorithm sorts a vector

; we only guarantee that each pass brings one more element into its correct position - the rightmost element of the current subproblem; there can be O(n) subproblems overall with each subproblem taking O(n) time, so overall time is O(n ^ 2)

; actually, the ith subproblem takes O(n - i) time so that we have overall time that is sum of first n integers (starting at one), which is O(n * (n + 1) / 2) = O(n ^ 2)

; we ought to prove each iteration of bubble sort leads largest value in current subproblem to end at right of current subproblem; we do so via induction for the pair-wise swaps; the current pair includes the largest value seen for all pairs for this iteration so that once we reach last pair for current iteration, largest value for this iteration ends at right

; SOME PRELIMINARY SPECIFICS

; iterations are 1-indexed

; specifically, iteration one affects items (1-indexed) 1 ... n or 
; pairs with left indexes 0 ... (n - 2)

; so, iteration i affects items (1-indexed) 1 ... (n - i + 1) or 
; pairs with left indices 0 ... (n - i - 1)

; n - i - 1 == 0 => i = n - 1

; we have number of iterations equal to max(0, n - 1)

; within an ith iteration, we have number of pairs considered equal to n - i

; subiterations are 1-indexed

; for ith iteration, subiterations are numbered 1 ... n - i

; PROOF THAT EACH ITERATION MOVES LARGEST VALUE
; IN SUBPROBLEM TO RIGHT-MOST POSITION FOR SUBPROBLEM

; proof that ith iteration (1-indexed) leads
; to max. value for current sub-problem being moved
; to right-most position (index, 1-indexed, of n - i + 1)
; for current sub-problem via induction

; basis step (i.e. for j == 1):
; before any subiteration for current iteration has started, 
; value at left-most position is largest for indices (1-indexed) 1 ... 1

; inductive step:
; hypothesis (i.e. for iteration j for j >= 1):
; assume that before jth subiteration (1-indexed),
; numbers for indices are s.t. max number for range
; of indices (1-indexed) 1 ... j is at index j

; conclusion (i.e. for iteration j for j >= 1):
; via swapping, after jth subiteration (1-indexed),
; numbers for indices are s.t. max number for range
; of indices (1-indexed) 1 ... (j + 1) is at index j + 1

; (this runs for n - i iterations, excluding basis step as an iteration)

; Q.E.D.

; PROOF THAT BY RUNNING MAX(0, n - 1) ITERATIONS, 
; WE END UP WITH SORTED VECTOR

; proof that after i iterations (1-indexed),
; all indices in vector are sorted in non-decreasing (sorted) order

; basis step (i.e. for i == 1):
; before any iteration has started, 
; have zero right-most values in non-decreasing (sorted) order

; inductive step: 
; hypothesis (i.e. for iteration i for i >= 1):
; assume that before ith iteration (1-indexed), 
; numbers for indices (1-indexed) (n - i + 2) ... n 
; are in non-decreasing (sorted) order

; conclusion (i.e. for iteration i for i >= 1):
; via prior proof, we move next largest value
; by inspecting indices (1-indexed) 1 ... (n - i + 1)
; to index (1-indexed) n - i + 1

; (this runs for n - 1 iterations, excluding basis step as an iteration)

; Q.E.D.

; c) what is the running time order of growth?

; O(n ^ 2)

; extra

; 1.

; ex. 3.19

; cycle detection using O(1) space

; use floyd's tortoise-and-hare algorithm

; one key is that we are assuming out-degree for each node is at most one

; also key is that because the hare moves twice as fast as tortoise 
; and that both begin at same place, if there is a cycle, 
; at some point the hare laps the tortoise

; a third key is that with tortoise-hare in presence of a cycle, 
; upon entering cycle, distance between tortoise and hare decreases by one 
; every tick, so first time we meet, we actually meet

; the sum of length of lead-up and size of cycle are in O(n)

; we have justified first phase, of identifying whether we have a cycle; 
; now, we just need to justify last two phases - finding mu (cycle start offset) 
; and finding lambda (cycle size)

; for second phase, based on geometry, we use where hare and tortoise meet, 
; move hare to start of list, advance hare and tortoise at same rate 
; s.t. both move one node per tick, and the first tick s.t. the hare and tortoise 
; meet with this modified configuration gives us mu

; the third phase is easiest - given mu, we use one pointer 
; and keep moving one node per tick until we reach the same node 
; as that associated with offset mu; this gives us lambda

; from ex. 3.12

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; from ex. 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define nc-lst (list 'a 'b 'c))
(define c-lst (make-cycle (list 'a 'b 'c)))

; (display z)

; for ex. 3.18

; takes time quadratic in n since we are not using hash table

; uses make-pointer-set, element-of-pointer-set?, adjoin-pointer-set from ex. 3.17

(define (non-constant-space-has-cycle l)
  (define curr-shallow-set (make-pointer-set nil))
  (define (ncscd-helper l)
    (if (null? l)
	#f
	(if (equal? (element-of-pointer-set? l curr-shallow-set) #f)
	    (begin
	      (set! curr-shallow-set (adjoin-pointer-set l curr-shallow-set))
	      (ncscd-helper (cdr l)))
	    #t)))
    (ncscd-helper l))

; for ex. 3.19

; we deviate from wikipedia article assumptions; 
; before, we assume we have n nodes and n edges; 
; this means that we always have a cycle; 
; however, the sicp problem states that we care about 
; cycles reachable via starting at head pointer 
; and following next pointers via cdr; 
; this means we don't care about cycles 
; not reachable from head and cycle might not exist; 
; to address possibility of cycle not existing, 
; we know ignoring first step (s.t. hare and tortoise are at head) 
; that if there is no cycle, tortoise never catches up to hare; 
; so if hare reaches a node s.t. next pointer is null, 
; then there is no cycle; then, for first step, 
; if next pointer is null, again there is no cycle; 
; so for every step we always check if hare is at node 
; s.t. next pointer is null and if it is, 
; there is no cycle reachable from head

; inspired by wikipedia article on cycle detection

(define (floyd l)
  (define (f pair)
    (cdr pair))
  ; if we ever reach a node s.t. next pointer is nil, 
  ; we return -1
  (define (double-f-with-last-node-detection x)
    (let ((next-node (f x)))
      (if (eq? next-node nil)
	  -1
	  (let ((next-next-node (f next-node)))
	    (if (eq? next-next-node nil)
		-1
		next-next-node)))))
  (define tortoise nil)
  (define hare nil)
  (define mu nil)
  (define lam nil)
  (define (loop1)
    (if (eq? tortoise hare)
	nil
	(begin
	  (set! tortoise (f tortoise))
	  (set! hare (double-f-with-last-node-detection hare))
	  (if (eq? hare -1)
	      -1
	      (loop1)))))
  (define (loop2)
    (if (eq? tortoise hare)
	nil
	(begin
	  (set! tortoise (f tortoise))
	  (set! hare (f hare))
	  (set! mu (+ mu 1))
	  (loop2))))
  (define (loop3)
    (if (eq? tortoise hare)
	nil
	(begin
	  (set! hare (f hare))
	  (set! lam (+ lam 1))
	  (loop3))))
  (if (eq? l nil)
      (list nil nil)
      (begin
	(set! tortoise (f l))
	(set! hare (double-f-with-last-node-detection l))
	(if (eq? hare -1)
	    (list nil nil)
	    (let ((result1 (loop1)))
	      (if (eq? result1 -1)
		  (list nil nil)
		  (begin
		    (set! mu 0)
		    (set! tortoise l)
		    (loop2)
		    (set! lam 1)
		    (set! hare (f tortoise))
		    (loop3)
		    (list lam mu))))))))

(define graph-list (cons 1 (cons 2 (cons 3 nil))))

(display graph-list)
(display "\n")

(display (floyd graph-list))
(display "\n")

(define (turnValueIndexPairVectorToPairs index-vec)
  (define vec-length (vector-length index-vec))
  (define pair-vec (make-vector vec-length))
  (define (loop1 i)
    (if (< i 0)
	pair-vec
	(begin
	  (vector-set! pair-vec i (cons (car (vector-ref index-vec i)) nil))
	  (loop1 (- i 1)))))
  (define (loop2 i)
    (if (< i 0)
	pair-vec
	(let ((have-next-index (not (eq? (cdr (vector-ref index-vec i)) nil))))
	  (begin
	    (if (eq? have-next-index #f)
		nil
		(let ((next-index (cadr (vector-ref index-vec i))))
		  (set-cdr! (vector-ref pair-vec i) (vector-ref pair-vec next-index))))
	    (loop2 (- i 1))))))
  (loop1 (- vec-length 1))
  (loop2 (- vec-length 1))
  (if (eq? vec-length 0)
      nil
      (vector-ref pair-vec 0)))

(define vip-vec (vector '(a 1) '(b 0) '(c 5) '(d 0) '(e 0) (cons 'f nil)))
(display vip-vec)
(display "\n")
(define prepared-l (turnValueIndexPairVectorToPairs vip-vec))
(display (floyd prepared-l))
(display "\n")


(define vip-vec2 (vector '(a 1) '(b 2) '(c 3) '(d 4) (cons 'f nil)))
(display vip-vec2)
(display "\n")
(define prepared-l2 (turnValueIndexPairVectorToPairs vip-vec2))
(display (floyd prepared-l2))
(display "\n")

1(define vip-vec3 (vector '(a 6) '(b 6) '(c 0) '(d 1) '(e 4) '(f 3) '(g 3) '(h 4) '(i 0)))
(display vip-vec3)
(display "\n")
(define prepared-l3 (turnValueIndexPairVectorToPairs vip-vec3))
(display (floyd prepared-l3))
(display "\n")

(define vip-vec4 (vector '(a 5) '(b 0) '(c 0) '(d 0) '(e 0) '(f 0)))
(display vip-vec4)
(display "\n")
(define prepared-l4 (turnValueIndexPairVectorToPairs vip-vec4))
(display (floyd prepared-l4))
(display "\n")

(define vip-vec5 (vector '(a 1) '(b 0) '(c 5) '(d 0) '(e 0) (cons 'f nil)))
(display vip-vec5)
(display "\n")
(define prepared-l5 (turnValueIndexPairVectorToPairs vip-vec5))
(display (floyd prepared-l5))
(display "\n")

(define vip-vec6 (vector '(a 0)))
(display vip-vec6)
(display "\n")
(define prepared-l6 (turnValueIndexPairVectorToPairs vip-vec6))
(display (floyd prepared-l6))
(display "\n")

(define vip-vec7 (vector (cons 'a nil) '(b 1)))
(display vip-vec7)
(display "\n")
(define prepared-l7 (turnValueIndexPairVectorToPairs vip-vec7))
(display (floyd prepared-l7))
(display "\n")

(define vip-vec8 (vector (cons 'a nil)))
(display vip-vec8)
(display "\n")
(define prepared-l8 (turnValueIndexPairVectorToPairs vip-vec8))
(display (floyd prepared-l8))
(display "\n")

(define vip-vec9 (vector))
(display vip-vec9)
(display "\n")
(define prepared-l9 (turnValueIndexPairVectorToPairs vip-vec9))
(display (floyd prepared-l9))
(display "\n")

; ex. 3.23

; implement a deque with O(1)-time operations

; implement a doubly-linked list first

; (define-class (A element)
;   (method (getElementA)
; 	  element))
; (define-class (B element)
;   (parent (A element))
;   (method (getElementB)
; 	  element)
;   (method (setElementB nextElement)
; 	  (set! element nextElement)))
; (define B (instantiate B 1))
; (display (ask B 'setElementB 2))
; (display (ask B 'getElementA))
; (display (ask B 'getElementB))

; via goodrich & tamassia

(define-class (Position element)
  (method (setElement newElement)
	  (set! element newElement)))

(define-class (DNode prev next element)
  (parent (Position element))
  (method (getNext) next)
  (method (getPrev) prev)
  (method (setNext newNext)
	  (set! next newNext))
  (method (setPrev newPrev)
	  (set! prev newPrev)))

(define-class (DNodePositionList)
  (instance-vars
   (numElts 0)
   (header nil)
   (trailer nil))
  (initialize
   (set! header (instantiate DNode nil nil nil))
   (set! trailer (instantiate DNode header nil nil))
   (ask header 'setNext trailer))
  (method (size) numElts)
  (method (isEmpty) (eq? numElts 0))
  (method (first)
	  (if (eq? (ask self 'isEmpty) #t)
	      (error "list is empty")
	      (ask header 'getNext)))
  (method (last)
	  (if (eq? (ask self 'isEmpty) #t)
	      (error "list is empty")
	      (ask trailer 'getPrev)))
  (method (next p)
	  (let ((curr-next (ask p 'getNext)))
	    (if (eq? curr-next trailer)
		(error "cannot advance past the end of the list")
		curr-next)))
  (method (prev p)
	  (let ((curr-prev (ask p 'getPrev)))
	    (if (eq? curr-prev header)
		(error "cannot advance past the beginning of the list")
		curr-prev)))
  (method (addFirst e)
	  (set! numElts (+ numElts 1))
	  (let ((newNode (instantiate DNode header (ask header 'getNext) e)))
	    (ask (ask header 'getNext) 'setPrev newNode)
	    (ask header 'setNext newNode)))
  (method (addLast e)
	  (set! numElts (+ numElts 1))
	  (let ((newNode (instantiate DNode (ask trailer 'getPrev) trailer e)))
	    (ask (ask trailer 'getPrev) 'setNext newNode)
	    (ask trailer 'setPrev newNode)))
  (method (addAfter p e)
	  (set! numElts (+ numElts 1))
	  (let ((newNode (instantiate Dnode p (ask p 'getNext) e)))
	    (ask (ask p 'getNext) 'setPrev newNode)
	    (ask p 'setNext newNode)))
  (method (addBefore p e)
	  (set! numElts (+ numElts 1))
	  (let ((newNode (instantiate DNode (ask p 'getPrev) p e)))
	    (ask (ask p 'getPrev) 'setNext newNode)
	    (ask p 'setPrev newNode)))
  (method (remove p)
	  (set! numElts (- numElts 1))
	  (let ((vPrev (ask p 'getPrev))
		(vNext (ask p 'getNext)))
	    (ask vPrev 'setNext vNext)
	    (ask vNext 'setPrev vPrev)
	    (let ((vElem (ask p 'element)))
	      (ask p 'setNext nil)
	      (ask p 'setPrev nil)
	      vElem)))
  (method (set p e)
	  (let ((oldElt (ask p 'element)))
	    (ask p 'setElement e)
	    oldElt)))

(define l1 (instantiate DNodePositionList))
(display (ask l1 'size))
(display "\n")

(ask l1 'addFirst 8)
(display (ask l1 'size))
(display "\n")

(define n1 (ask l1 'first))
(display (ask n1 'element))
(display "\n")

(ask l1 'addAfter n1 5)
(display (ask l1 'size))
(display "\n")

(define n2 (ask l1 'next n1))
(display (ask n2 'element))
(display "\n")

(ask l1 'addBefore n2 3)
(display (ask l1 'size))
(display "\n")

(define n3 (ask l1 'prev n2))
(display (ask n3 'element))
(display "\n")

(ask l1 'addFirst 9)
(display (ask l1 'size))
(display "\n")

(display (ask (ask l1 'last) 'element))
(display "\n")

(ask l1 'remove (ask l1 'first))
(display (ask l1 'size))
(display "\n")

(define oldValue (ask l1 'set n3 7))
(display oldValue)
(display "\n")

(ask l1 'addAfter n1 2)
(display (ask l1 'size))
(display "\n")

(ask l1 'addLast 6)
(display (ask l1 'size))
(display "\n")

(ask l1 'remove (ask l1 'last))
(ask l1 'remove (ask l1 'last))
(ask l1 'remove (ask l1 'last))
(ask l1 'remove (ask l1 'last))
(display (ask l1 'isEmpty))
(display "\n")
(ask l1 'remove (ask l1 'last))
(display (ask l1 'isEmpty))
(display "\n")

; effectively implement make-deque, empty-deque?, front-deque, rear-deque, front-insert-deque!, rear-insert-deque!, front-delete-deque!, rear-delete-deque!

(define-class (Deque)
  (instance-vars
   (dllist (instantiate DNodePositionList)))
  (method (size)
	  (ask dllist 'size))
  (method (empty?)
	  (ask dllist 'isEmpty))
  (method (front)
	  (ask (ask dllist 'first) 'element))
  (method (rear)
	  (ask (ask dllist 'last) 'element))
  (method (front-insert! item)
	  (ask dllist 'addFirst item))
  (method (rear-insert! item)
	  (ask dllist 'addLast item))
  (method (front-delete!)
	  (ask dllist 'remove (ask dllist 'first)))
  (method (rear-delete!)
	  (ask dllist 'remove (ask dllist 'last))))

(define d1 (instantiate Deque))
(display (ask d1 'size))
(display "\n")

(ask d1 'front-insert! 3)
(ask d1 'front-insert! 5)
(display (ask d1 'front-delete!))
(display "\n")
(ask d1 'rear-insert! 7)
(display (ask d1 'front-delete!))
(display "\n")
(display (ask d1 'rear-delete!))
(display "\n")
; (ask d1 'front-delete!)
(display (ask d1 'empty?))
(display "\n")

; 2.

; cxr-name

; we settle for an algorithm that takes time quadratic in number of car's or cdr's composed

; the problem is harder than for the inverse function cxr-function

; the given function is a black box and we can't redefine car and cdr
; even temporarily; we are dealing with inert pairs and 
; we cannot (for example) interleave function calls in between the 
; car's and cdr's for the given function

; the problem would not be as hard if we knew a max. depth as we could
; manifest a tree with number of nodes exponential in this depth

; we also are avoiding turning the given function
; into a string that we parse

; we are able to get outermost operator identity (i.e. car or cdr); 
; but we are not interested in this as it is hard to construct
; a result using these results

; instead, we are interested in getting innermost operator identity 
; (i.e. car or cdr) and then we second-innermost operators and then 
; third-innermost operators until we run out of input composed 
; car's or cdr's; we allow for this by modifying the gadget that we 
; run the given function on so that it grows and has self-loops 
; s.t. we can identify and remember the next operator and so that
; if we don't reach the frontier, we also can recognize this 
; using special symbols and we know to stop

(define (cxr-name f)
  #t)

(display (cxr-name (lambda (x) (cadr (cddar (cadar x))))))

; have cycles

(define gadget
  (cons nil nil))

(define car-gadget
  (cons nil nil))

(define cdr-gadget
  (cons nil nil))

(set-car! gadget car-gadget)
(set-cdr! gadget cdr-gadget)
(set-car! car-gadget car-gadget)
(set-cdr! car-gadget cdr-gadget)
(set-car! cdr-gadget car-gadget)
(set-cdr! cdr-gadget cdr-gadget)

(display (eq? (car gadget) car-gadget))

(display (eq? (cadar gadget) car-gadget))

