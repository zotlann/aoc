(load "../../lib/lib.scm")
(require srfi/1)

(define input (file->string (open-input-file "input.txt")))
(define input (str-split input "\n"))
(define input (map string->list input))

(define (take lst n)
  (cond
    ((or (null? lst) (zero? n)) '())
    (else (cons (car lst) (take (cdr lst) (- n 1))))))

(define (drop lst n)
  (cond
    ((or (null? lst) (zero? n)) lst)
    (else (drop (cdr lst) (- n 1)))))

(define (split lst)
  (let ((half (/ (length lst) 2)))
   (list (take lst half) (drop lst half))))

(define (member? lst tar)
  (cond
    ((null? lst) #f)
    ((equal? (car lst) tar) #t)
    (else (member? (cdr lst) tar))))

(define (common l1 l2)
  (cond
    ((null? l1) #f)
    ((member? l2 (car l1)) (car l1))
    (else (common (cdr l1) l2))))

(define (split-3 lst)
  (if (null? lst)
      '()
      (cons (take lst 3) (split-3 (drop lst 3)))))

(define (get-score char)
  (if (< (char->integer char) 91)
      (- (char->integer char) 38)
      (- (char->integer char) 96)))

(define (common2 l1 l2 l3)
  (cond
    ((null? l1) #f)
    ((and (member? l2 (car l1)) (member? l3 (car l1))) (car l1))
    (else (common2 (cdr l1) l2 l3))))

(define input (split-3 input))
(define input (map (lambda (x) (common2 (car x) (list-ref x 1) (list-ref x 2))) input))
(display input)
(apply + (map get-score input))
