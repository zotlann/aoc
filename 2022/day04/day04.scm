(load "../../lib/lib.scm")

(define input (file->string (open-input-file "input.txt")))
(define input (str-split input "\n"))
(define input (map (lambda (x) (str-split x ",")) input))

(define (full-contain? section1 section2)
  (or (and (<= (car section2) (car section1))
           (>= (cadr section2) (car section1)))
      (and (<= (car section1) (car section2))
           (>= (cadr section1) (car section2)))))

(define (split lst)
  (let ((pair1 (str-split (car lst) "-"))
        (pair2 (str-split (cadr lst) "-")))
    (list (list (string->number (car pair1))
                (string->number (cadr pair1)))
          (list (string->number (car pair2))
                (string->number (cadr pair2))))))

(define input (map split input))

(define (day01-part1 input)
  (cond
    ((null? input) 0)
    ((full-contain? (caar input) (cadr (car input))) (+ 1 (day01-part1 (cdr input))))
    (else (day01-part1 (cdr input)))))


(define input (map (lambda (x) (if (full-contain? (car x) (cadr x)) 1 0)) input))

(apply + input)
