(load "../../lib/lib.scm")

(define stack1 (string->list "PGRN"))
(define stack2 (string->list "CDGFLBTJ"))
(define stack3 (string->list "VSM"))
(define stack4 (string->list "PZCRSL"))
(define stack5 (string->list "QDWCVLSP"))
(define stack6 (string->list "SMDWNTC"))
(define stack7 (string->list "PWGDH"))
(define stack8 (string->list "VMCSHPLZ"))
(define stack9 (string->list "ZGWLFPR"))

(define input (list stack1 stack2 stack3 stack4 stack5 stack6 stack7 stack8 stack9))

(define commands (file->string (open-input-file "commands.txt")))
(define commands (str-split commands "\n"))
(define commands (map (lambda (x) (str-split x " ")) commands))
(define commands (map (lambda (x) (map string->number x)) commands))

(define (get-string stack)
  (list->string (map car stack)))

(define (big-move n from to stacks)
  (if (zero? n)
      stacks
      (big-move (- n 1) from to (move from to stacks))))

(define (move from to stacks)
    (list-set! stacks (- to 1) (cons (car (list-ref stacks (- from 1))) (list-ref stacks (- to 1))))
    (list-set! stacks (- from 1) (cdr (list-ref stacks (- from 1))))
    stacks)

(define (take n lst)
  (if (or (null? lst) (zero? n))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (null? lst) (zero? n))
      lst
      (drop (- n 1) (cdr lst))))

(define (biggest-move n from to stacks)
  (list-set! stacks (- to 1) (append (take n (list-ref stacks (- from 1))) (list-ref stacks (- to 1))))
  (list-set! stacks (- from 1) (drop n (list-ref stacks (- from 1))))
  stacks)

(define (day05-part1 commands stacks)
  (if (null? commands)
      (get-string stacks)
      (let ((command (car commands)))
       (day05-part1 (cdr commands) (biggest-move (list-ref command 0) (list-ref command 1) (list-ref command 2) stacks)))))

(display (day05-part1 commands input))
