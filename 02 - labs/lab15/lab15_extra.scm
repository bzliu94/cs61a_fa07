(define (last-pair l)
  (last-pair-helper l))

(define (last-pair-helper l)
  (if (equal? (cdr l) nil)
    l
    (last-pair-helper (cdr l))))


