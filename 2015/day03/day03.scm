(load "../../lib/lib.scm")

(require openssl/md5)

(define input (file->string (open-input-file "input.txt")))

(define (move current-location dir)
  (let ((x (car current-location))
        (y (cadr current-location)))
    (cond
      ((char=? #\^ dir) (list x (+ y 1)))
      ((char=? #\v dir) (list x (- y 1)))
      ((char=? #\< dir) (list (- x 1) y))
      ((char=? #\> dir) (list (+ x 1) y))
      (else (list x y)))))

(define (day01-part1 input)
  (define (helper input current visited)
    (if (null? input)
        (length visited)
        (let ((new (move current (car input))))
         (helper (cdr input) new (set-add visited new)))))
  (helper input (list 0 0) '((0 0))))

(define (day01-part2 input)
  (define (helper input santa robot visited mover)
    (cond
      ((null? input) (length visited))
      ((eq? mover 'santa)
       (let ((new (move santa (car input))))
        (helper (cdr input) new robot (set-add visited new) 'robot)))
      ((eq? mover 'robot)
       (let ((new (move robot (car input))))
        (helper (cdr input) santa new (set-add visited new) 'santa)))))
  (helper input '(0 0) '(0 0) '((0 0)) 'santa))

(define (set-add set x)
  (cond
    ((null? set) (list x))
    ((member x set) set)
    (else (cons x set))))

(define input (string->list input))
(day01-part1 input)
