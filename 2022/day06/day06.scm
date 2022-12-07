(load "../../lib/lib.scm")

(define input (file->string (open-input-file "input.txt")))
(define input (string->list input))

(define (member? tar lst)
  (cond
    ((null? lst) #f)
    ((equal? (car lst) tar) #t)
    (else (member? tar (cdr lst)))))
(define (remove-end lst)
  (define (helper lst n)
    (if (or (null? lst) (zero? n))
        '()
        (cons (car lst) (helper (cdr lst) (- n 1)))))
  (helper lst (- (length lst) 1)))
(define (set? lst)
  (cond
    ((null? lst) #t)
    ((member? (car lst) (cdr lst)) #f)
    (else (set? (cdr lst)))))
(define (list-n item n)
  (cond
    ((zero? n) '())
    (else (cons item (list-n item (- n 1))))))
(define (drop n lst)
  (cond
    ((or (null? lst) (zero? n)) lst)
    (else (cons (car lst) (drop (- n 1) (cdr lst))))))

(define (take n lst)
    (if (or (zero? n) (null? lst))
        '()
        (cons (car lst) (take (- n 1) (cdr lst)))))

(define (day06-part1 input)
  (define (helper input last n)
    (if (null? input)
         #f
         (let ((new-last (append (list (car input)) (remove-end last))))
          (if (set? new-last)
              n 
              (helper (cdr input) new-last (+ n 1))))))
  (helper (drop 14 input) (take 14 input) 1))

(day06-part1 input)
