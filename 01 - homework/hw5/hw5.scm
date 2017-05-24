; 1. 

; ex. 2.24

; (list 1 (list 2 (list 3 4)))

; interpreter result:
; (1 (2 (3 4)))

; box-and-pointer structure:
;           _ _     _ _ 
; list  -> | | |-> | |/|
;          |_|_|   |_|_|
;           |       |
;           v       |
;           1       |
;                   v _     _ _
;                  | | |-> | |/|
;                  |_|_|   |_|_|
;                   |       |
;                   v       |
;                   2       |
;                           v _     _ _
;                          | | |-> | |/|
;                          |_|_|   |_|_|
;                           |       |
;                           v       v
;                           3       4

; tree interpretation:

; 	(1 (2 (3 4)))
; 	     / \
; 	    /   \
; 	   /     \
; 	  /    (2 (3 4))
;	 /       / \
;       /       /   \
;      /       /     \
;     /	      /	    (3 4)
;    /	     /	     / \
;   /	    /	    /   \
;  /	   /	   /     \
; 1	  2	  3       4

; ex. 2.26

; (define x (list 1 2 3))
; (define y (list 4 5 6))

; (append x y)
; -> (1 2 3 4 5 6)
; (cons x y)
; -> ((1 2 3) 4 5 6)
; (list x y)
; -> ((1 2 3) (4 5 6))

; ex. 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; b.

(define (is-branch? item)
  (number? (car item)))

(define (is-mobile? item)
  (not (number? (car item))))

(define (total-weight mobile)
  (total-weight-helper mobile))

; alternating mobile/branch statuses

(define (total-weight-helper item)
  (cond ((is-mobile? item)
	 (+ (total-weight-helper (left-branch item))
	    (total-weight-helper (right-branch item))))
	((is-branch? item)
	 (if (number? (branch-structure item))
	     (branch-structure item)
	     (total-weight-helper (branch-structure item))))))

(define m (make-mobile
	   (make-branch 2
			(make-mobile
			 (make-branch 2 1)
			 (make-branch 2 1)))
	   (make-branch 2 2)))

(define w (total-weight m))

; c.

(define (is-balanced-shallow? mobile)
  (is-balanced-shallow?-helper mobile))

; takes linear time

(define (is-balanced-shallow?-helper mobile)
  (let ((left-b (left-branch mobile))
	(right-b (right-branch mobile)))
    (let ((left-distance (branch-length left-b))
	  (right-distance (branch-length right-b)))
      (let ((left-moment (* left-distance (total-weight left-b)))
	    (right-moment (* right-distance (total-weight right-b))))
	(equal? left-moment right-moment)))))

; takes quadratic time

(define (is-balanced-deep? mobile)
  (is-balanced-deep?-helper mobile))

(define (is-balanced-deep?-helper item)
  (cond ((is-mobile? item)
	 (and (is-balanced-shallow? item)
	      (is-balanced-deep? (left-branch item))
	      (is-balanced-deep? (right-branch item))))
	((is-branch? item)
	 (if (number? (branch-structure item))
	     #t
	     (is-balanced-deep?-helper (branch-structure item))))))

; takes quadratic time

(define (is-balanced mobile)
  (is-balanced-deep? mobile))
      
; d.

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; required modifications

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

; ex. 2.30

(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square x)
  (* x x))

(define (square-tree1 tree)
  (square-tree1-helper tree))

(define (square-tree1-helper tree)
  (if (null? tree)
      nil
      (if (list? (car tree))
	  (cons (square-tree1-helper (car tree))
		(square-tree1-helper (cdr tree)))
	  (cons (square (car tree))
		(square-tree1-helper (cdr tree))))))

(define (square-tree2 tree)
  (square-tree2-helper tree))

(define (square-tree2-helper tree)
  (cond ((null? tree) nil)
	((number? tree) (square tree))
	((list? tree)
	  (map square-tree2-helper tree))))

; ex. 2.31

(define (tree-map f t)
  (tree-map-helper f t))

(define (tree-map-helper f t)
  (cond ((null? t) nil)
	((not (list? t)) (f t))
	((list? t)
	 (map (lambda (x) (tree-map-helper f x)) t))))

(define (square-tree3 tree)
  (tree-map square tree))

; ex. 2.32

; complete definition of a method that takes a set as a list of distinct elements and gives all subsets in the form of list of lists; explain why it works

(define (subsets s)
  (if (null? s)
      (list nil) ; works because including nothing is a valid subset
      (let ((rest (subsets (cdr s)))) ; recursive retrieval of subsets for set of items to right of current element
	(append rest (map (lambda (x) (append (list (car s)) x)) rest))))) ; rest (left operand) gives "do-not-include" and right operand gives "do-include"

					; works because input elements are unique and we have a decision for each element - include or do not include

; (subsets (list 1 2 3))
; -> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; ex. 2.36

; grouping in orthogonal direction

(define (accumulate op init seq)
  (if (equal? seq nil) init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (cond ((null? seqs) nil)
	((null? (car seqs)) nil)
	(else (cons (accumulate op init (map (lambda (x) (car x)) seqs))
		    (accumulate-n op init (map (lambda (x) (cdr x)) seqs))))))

; ((1 2 3) (4 5 6) (7 8 9) (10 11 12))

; (accumulate-n + 0 s)
; -> (22 26 30)

; ex. 2.37

; ((1 2 3 4) (4 5 6 6) (6 7 8 9))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v '(1 2 3)) ; 1 x 3; row vector
(define w '(2 3 4)) ; 1 x 3; row vector
(define m '((1 2 3) (4 5 6))) ; 2 x 3
(define n '((1 2) (3 4) (5 6))) ; 3 x 2

; (dot-product v w)
; -> (+ 2 6 12)
; -> 20

; take second argument (which ought to be a column vector)
; in the form of a row vector; assume output is a row vector
(define (matrix-*-vector m v-transpose)
  (map (lambda (x) (dot-product x v-transpose)) m))

; (matrix-*-vector m v)
; -> ((+ 1 4 9) (+ 4 10 18))
; -> (14 32)

(define (listify x)
  (if (list? x)
      x
      (list x)))

; only works for matrices

(define (transpose mat)
  (accumulate-n (lambda (x y)
		  (let ((next-x (listify x))
			(next-y (listify y)))
		    (append next-x next-y)))
		nil
		mat))

(define (transpose-vector v)
  (map (lambda (x) (list x)) v))

; (transpose m)
; -> ((1 4) (2 5) (3 6))

; rows of m interact with columns of n

; n transpose on left, m tranpose on right

; M * N = N ^ T * M ^ T

; first row pre-transpose is first column post-transpose

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

; (matrix-*-matrix m n)
; -> ((22 28) (49 64))

; ex. 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
	(iter (op (car rest) result)
	      (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
; -> 1.5
(fold-left / 1 (list 1 2 3))
; -> 0.166...
(fold-right list nil (list 1 2 3))
; (3 (2 (1 ())))
(fold-left list nil (list 1 2 3))
; (((() 1) 2) 3)

; op should satisfy commutative property to ensure that fold-right and fold-left will produce same values for any sequence

; ex. 2.54

; define equal? for lists that works in a deep manner
; in terms of eq?

(define (equal? item1 item2)
  (equal?-helper item1 item2))

(define (equal?-helper item1 item2)
  (cond ((and (symbol? item1) (symbol? item2))
	 (eq? item1 item2))
	((and (list? item1) (list? item2))
	 (if (or (null? item1) (null? item2))
	     (eq? (null? item1) (null? item2))
	     (and (equal?-helper (car item1) (car item2))
		  (equal?-helper (cdr item1) (cdr item2)))))
	(else #f)))

(equal? '(this is a list) '(this is a list))
; -> #t

(equal? '(this is a list) '(this (is a) list))
; -> #f

; 2. 

; see "calc_modified.scm"

; extra

; 1.

; ex. 2.67

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (decode sample-message sample-tree)
; -> (a d a b b c a)

; ex. 2.68

; Given a Huffman tree, we can find the encoding of any symbol by starting at the root and moving down until we reach the leaf that holds the symbol. Each time we move down a left branch we add a 0 to the code, and each time we move down a right branch we add a 1. (We decide which branch to follow by testing to see which branch either is the leaf node for the symbol or contains the symbol in its set.) For example, starting from the root of the tree in figure 2.18, we arrive at the leaf for D by following a right branch, then a left branch, then a right branch, then a right branch; hence, the code for D is 1011.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (encode-symbol-helper symbol tree))

(define (encode-symbol-helper symbol tree)
  (if (leaf? tree)
      nil
      (let ((left (left-branch tree))
	    (right (right-branch tree)))
	(cond ((member? symbol (symbols left))
	       (cons 0 (encode-symbol-helper symbol left)))
	      ((member? symbol (symbols right))
	       (cons 1 (encode-symbol-helper symbol right)))
	      (else (error "symbol not in tree"))))))

; (encode '(a d a b b c a))
; -> (0 1 1 0 0 1 0 1 0 1 1 1 0)

; ex. 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; makes list of leaves in increasing order of weight

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

; we assume that there are at least two symbols in huffman tree

(define (successive-merge items)
  (car (successive-merge-helper (reverse items))))

; The algorithm for generating a Huffman tree is very simple. The idea is to arrange the tree so that the symbols with the lowest frequency appear farthest away from the root. Begin with the set of leaf nodes, containing symbols and their frequencies, as determined by the initial data from which the code is to be constructed. Now find two leaves with the lowest weights and merge them to produce a node that has these two nodes as its left and right branches. The weight of the new node is the sum of the two weights. Remove the two leaves from the original set and replace them by this new node. Now continue this process. At each step, merge two nodes with the smallest weights, removing them from the set and replacing them with a node that has these two as its left and right branches. The process stops when there is only one node left, which is the root of the entire tree.

(define (merge item1 item2)
  (make-code-tree item1 item2))

(define (have-lower-weight items w)
  (filter (lambda (x) (< (weight x) w)) items))

(define (have-matching-weight items w)
  (filter (lambda (x) (= (weight x) w)) items))

(define (have-higher-weight items w)
  (filter (lambda (x) (> (weight x) w)) items))

; take advantage of fact that post-reverse,
; two branches with lowest weights are towards left

; not efficient in terms of time needed; 
; maintaining sorted order is done in a brute-force manner; 
; takes quadratic time

(define (successive-merge-helper items)
  (if (= (length items) 1)
      items
      (let ((mergee-a (car items))
	    (mergee-b (cadr items))
	    (rest (cddr items)))
	(let ((result-item (merge mergee-a mergee-b)))
	  (let ((w (weight result-item)))
	    (let ((rest-left (have-lower-weight rest w))
		  (rest-center (have-matching-weight rest w))
		  (rest-right (have-higher-weight rest w)))
	      (successive-merge-helper (append rest-left (list result-item) rest-center rest-right))))))))

(define our-pairs '((A 4) (B 2) (C 1) (D 1)))

; ex. 2.70

(define rock-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define rock-tree (generate-huffman-tree rock-pairs))

; (display rock-tree)

(define message1 '(get a job))
(define message2 '(sha na na na na na na na na))
(define message3 '(get a job))
(define message4 '(sha na na na na na na na na))
(define message5 '(wah yip yip yip yip yip yip yip yip yip))
(define message6 '(sha boom))

(define combined-message (append message1 message2 message3 message4 message5 message6))

(define combined-code (encode combined-message rock-tree))

; (display (accumulate + 0 (map count combined-message)))

; (display (count combined-message)) ; 36 symbols
; (display (length combined-code)) ; 89 bits required for 36 symbols

; for fixed-length code for eight-symbol alphabet, we would expect
; three bits per symbol; this means 36 * 3 = 108 bits

; ex. 2.71

;     |
;    / \
;   / \ \
;  / \ \ \
; / \ \ \ \
; 1 2 4 8 16

; number of symbols? 5
; bits for most frequent symbol? 1
; bits for least frequent symbol? 4

;                   |
;                  / \
;                 /   \
;                / \   \
;               /   \   \
;              / \   \   \
;             /   \   \   \
;            / \   \   \   \
;           /   \   \   \   \
;          / \   \   \   \   \
;         /   \   \   \   \   \
;        / \   \   \   \   \   \
;       /   \   \   \   \   \   \
;      / \   \   \   \   \   \   \
;     /   \   \   \   \   \   \   \
;    / \   \   \   \   \   \   \   \
;   /   \   \   \   \   \   \   \   \
;  / \   \   \   \   \   \   \   \   \
; /   \   \   \   \   \   \   \   \   \ 
; 1   2   4   8   16  32  64  128 256 512

; number of symbols? 10
; bits for most frequent symbol? 1
; bits for least frequent symbol? 9

; in general:
; bits for most frequent symbol? 1
; bits for least frequent symbol? n - 1

; ex. 2.72

; complexity required for our implementation of encode?
; number of levels is in O(n) and
; checking for inclusion at each level takes O(n) time
; if tree is as unbalanced as is supposed in problem 2.71;
; this implies O(n ^ 2) time for encoding one symbol

; encoding most frequent symbol? O(n) time
; encoding least frequent symbol? O(n ^ 2) time

; 2.

; see regroup.scm


