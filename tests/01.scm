(define (reverse1 xs)
	(define (helper xs rs)
		(if (null? xs)
			rs
			(helper (cdr xs) (cons (car xs) rs))))
	(helper xs (list)))
(display (reverse (list 1 2 3 4.023 5 "Hello, World!" #t #\A #b1101101001)))
(newline)
(display (reverse1 (list 1 2 3 4.023 5 "Hello, World!" #t #\A #b1101101001)))
(newline)

(define (append xs ys) 
	(define (helper xs ys) 
		(if (null? xs) 
			ys 
			(helper (cdr xs) (cons (car xs) ys))))
	(helper (reverse xs) ys))
	
(display (append (list 1 2 3) (list 4 5 6)))
