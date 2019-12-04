(define file->list
  (lambda (filename)
    (define helper
      (lambda (file-port)
	(let ((x (read file-port)))
	  (if (eof-object? x)
	      '()
	      (cons x (helper file-port))))))
  (helper (open-input-file filename))))

(define day01-calc
  (lambda (n)
    (define helper
      (lambda (n acc)
        (let ((fuel-req (- (floor (/ n 3)) 2)))
          (if (<= fuel-req 0)
	      acc
	      (helper fuel-req (+ acc fuel-req))))))
    (helper n 0)))


(define sum
  (lambda (lst)
    (if (null? lst)
	0
	(+ (car lst) (sum (cdr lst))))))



(define day01
  (lambda (filename)
    (define n-list (file->list filename))
    (sum (map day01-calc n-list))))

(day01 "input.txt")
