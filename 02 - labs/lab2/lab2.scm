(define (square x)
  (* x x))

;; larger values are a and b
;; larger values are a and c
;; larger values are b and c
;; (3 C 2) = 6 permutations, (3 R 2) = 3 combinations
(define (proc a b c)
  (cond ((and (>= a c) (>= b c)) (+ (square a) (square b)))
	((and (>= a b) (>= c b)) (+ (square a) (square c)))
	((and (>= b a) (>= c a)) (+ (square b) (square c)))))

(define (dupls-removed l)
  (dupls-removed-helper l ()))

(define (dupls-removed-helper l result)
  (if (= (count l) 0)
      result
      (if (member? (first l) result)
	  (dupls-removed-helper (bf l) result)
	  (dupls-removed-helper (bf l) (sentence (first l) result)))))
