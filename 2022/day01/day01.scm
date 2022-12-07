(load "~/Documents/aoc/lib/lib.scm")

(define input (file->string (open-input-file "~/Documents/aoc/2022/day01/input.txt")))
(define input (str-split input "\n\n"))
(define input (map (lambda (x) (str-split x "\n")) input))
(define input (map (lambda (x) (map string->number x)) input))

(define (day01-part1 input)
  (let ((sums (map (lambda (x) (apply + x)) input)))
   (apply max sums)))

(define (day01-part2 input)
  (let ((sorted-sums (sort (map (lambda (x) (apply + x)) input) >)))
   (apply + (take sorted-sums 3))))

(define (rember lst tar)
  (cond
    ((null? lst) '())
    ((equal? (car lst) tar) (rember (cdr lst) tar))
    (else (cons (car lst) (rember (cdr lst) tar)))))


(define (mcons->lst lst)
  (cond
    ((null? lst) '())
    (else (cons (car lst) (mcons->lst (cdr lst))))))

(define sums (map (lambda (x) (apply + x)) input))
sort
