(define (prime? n)
  (define (help-prime n a)
    (if (not(or (eq? (remainder n a) 0)
                (> a (quotient n 2))))
        (help-prime n (+ 1 a))
        (not (eq? (remainder n a) 0))))
  (help-prime n 2))
(display (prime? 23))

