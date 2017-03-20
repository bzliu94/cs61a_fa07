; 1.

; ex. 3.12

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))

;       _ _     _ _
; x -> | | |-> | |/|
;      |_|_|   |_|_|
;       |       |
;       v       v
;      'a      'b

(define y (list 'c 'd))

;       _ _     _ _
; y -> | | |-> | |/|
;      |_|_|   |_|_|
;       |       |
;       v       v
;      'c      'd

(define z (append x y))

;       _ _     _ _     _ _     _ _
; z -> | | |-> | | |-> | | |-> | |/|
;      |_|_|   |_|_|   |_|_|   |_|_|
;       |       |       |       |
;       v       v       v       v
;      'a      'b      'c      'd

; z
; (a b c d)

; (cdr x)
; (b)

(define w (append! x y))

; w
; |
; v     _ _     _ _     _ _     _ _
; x -> | | |-> | | |-> | | |-> | |/|
;      |_|_|   |_|_|   |_|_|   |_|_|
;       |       |       |       |
;       v       v       v       v
;      'a      'b      'c      'd

; w
; (a b c d)

; (cdr x)
; (b c d)

; 2.

(define x (cons 1 3))
(define y 2)

; incorrect
(set! (cdr x) y)

; correct
(set-cdr! x y)

; the reason the first expression is incorrect is that we're attempting to modify the value returned by a cdr call, which would instantly get thrown away; possibly for this reason, the documentation says that the first argument for set! must be a variable name

; 3a.

(define list1 (list (list 'a) 'b))
; produces ((a) b)

(define list2 (list (list 'x) 'y))
; produces ((x) y)
 
; goal:
; change (x) to (x b)
; then, change (a) to (a x b)

(set-cdr! (car list2) (cdr list1))
(set-cdr! (car list1) (car list2))

; 3b.

; (set-car! (cdr list1) (cadr list2))

; list1 is ((a x b) b)
; list2 is ((x b) y)

;           _ _     _ _
; list1 -> | | |-> | |/|
;          |_|_|   |_|_|
;           |       |
;           |       v
;           |      'b
;           v _
;          | |/|
;          |_|_|
;           |
;           v
;          'a

;           _ _     _ _
; list2 -> | | |-> | |/|
;          |_|_|   |_|_|
;           |       |
;           |       v
;           |      'y
;           v _
;          | |/|
;          |_|_|
;           |
;           v
;          'x

; BECOME (after first set-cdr!)

;           _ _     _ _
; list2 -> | | |-> | |/|
;          |_|_|   |_|_|
;           |       |
;           |       v
;           |      'y
;           v _
;          | | |-----
;          |_|_|    |
;           |       |
;           v       |
;          'x       |
;           _ _     v _
; list1 -> | | |-> | |/|
;          |_|_|   |_|_|
;           |       |
;           |       v
;           |      'b
;           v _
;          | |/|
;          |_|_|
;           |
;           v
;          'a

; BECOME (after second set-cdr!)

;              _ _     _ _
;    list2 -> | | |-> | |/|
;             |_|_|   |_|_|
;              |       |
;              |       v
;              |      'y
;              v _
;  ---------> | | |-----
;  |          |_|_|    |
;  |           |       |
;  |           v       |
;  |          'x       |
;  |           _ _     v _
;  | list1 -> | | |-> | |/|
;  |          |_|_|   |_|_|
;  |           |       |
;  |           |       v
;  |           |      'b
;  |           v _
;  |          | | |-----
;  |          |_|_|    |
;  |           |       |
;  |           v       |
;  |          'a       |
;  ---------------------


; (set-car! (cdr list1) (cadr list2))

; list1 is ((a x b) b)
; list2 is ((x b) y)
; (cdr list1) is (b)
; (cadr list2) is (car (cdr (list2))) is (car (y)) is y

;              _ _     _ _
;    list2 -> | | |-> | |/|
;             |_|_|   |_|_|
;              |       |
;              |       v
;              |      'y <---
;              v _          |
;  ---------> | | |-----    |
;  |          |_|_|    |    |
;  |           |       |    |
;  |           v       |    |
;  |          'x       |    |
;  |           _ _     v _  |
;  | list1 -> | | |-> | |/| |
;  |          |_|_|   |_|_| |
;  |           |       |    |
;  |           |       |    |
;  |           |       ------
;  |           v _
;  |          | | |-----
;  |          |_|_|    |
;  |           |       |
;  |           v       |
;  |          'a       |
;  ---------------------

; results:
; list1 is ((a x y) y)
; list2 is ((x y) y)

; 4.

; ex. 3.13

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(last-pair z)

; (a b c)

;       _ _     _ _     _ _
; y -> | | |-> | | |-> | |/|
;      |_|_|   |_|_|   |_|_|
;       |       |       |
;       v       v       v
;      'a      'b      'c

;
;       ----------------------
;       |                    |
;       v _     _ _     _ _  |
; z -> | | |-> | | |-> | | |--
;      |_|_|   |_|_|   |_|_|
;       |       |       |
;       v       v       v
;      'a      'b      'c

; if we try to compute (last-pair z), we get stuck in an infinite loop because cdr of the current list is never nil

; ex. 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

; the method mutates a list so that it is reversed

; BEFORE CALL

;       _ _     _ _     _ _     _ _
; v -> | | |-> | | |-> | | |-> | |/|
;      |_|_|   |_|_|   |_|_|   |_|_|
;       |       |       |       |
;       v       v       v       v
;      'a      'b      'c      'd

; v: (a b c d)

; AFTER CALL

;                           v ---
;                               |
;       _ _     _ _     _ _     v _
; w -> | | |-> | | |-> | | |-> | |/|
;      |_|_|   |_|_|   |_|_|   |_|_|
;       |       |       |       |
;       v       v       v       v
;      'd      'c      'b      'a

; v: (a)
; w: (d c b a)

