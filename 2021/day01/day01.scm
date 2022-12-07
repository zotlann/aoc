(load "../../lib/lib.scm")

(define (day01-part1 input)
  (cond
    ((or (null? input) (null? (cdr input))) 0)
    ((< (car input) (car (cdr input))) (+ 1 (day01-part1 (cdr input))))
    (else (day01-part1 (cdr input)))))

(define (day01-part2 input)
  (if (< (length input) 4)
      0
      (+ (is-bigger? input) (day01-part2 (cdr input)))))

(define (is-bigger? input)
  (let* ((a (list-ref input 0))
        (b (list-ref input 1))
        (c (list-ref input 2))
        (d (list-ref input 3))
        (n1 (+ a b c))
        (n2 (+ b c d)))
    (if (> n2 n1) 1 0)))

(define file (open-input-file "input.txt"))
(define file-str (file->string file))
(define input (map string->number (str-split file-str "\n")))
(day01-part2 input)
