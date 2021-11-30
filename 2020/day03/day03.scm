(define (file->string file)
  (define (helper file)
    (let ((x (read-char file)))
     (if (eof-object? x)
         '()
         (cons x (helper file)))))
  (list->string (helper file)))

(define (str-split str s)
  (let ((len (string-length str))
        (slen (string-length s)))
    (letrec
      ((split
         (lambda (a b)
           (cond
             ((> (+ b slen) len) (if (= a b) '() (cons (substring str a len) '())))
             ((= (+ b slen) len)
              (if (and (= slen 1) (string=? (substring str a len) s))
                  '()
                  (cons (substring str a len) '())))
             ((string=? s (substring str b (+ b slen)))
              (if (= a b)
                  (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split (+ b slen) (+ b slen)))))
             (else (split a (+ 1 b)))))))
      (split 0 0))))

(define (run-slope input dx dy)
  (define (helper input dx dy y)
    (cond
      ((null? input) 0)
      ((char=? (string-ref (car input) y) #\#) (+ 1 (helper (nth-cdr input dx) dx dy (remainder (+ y dy) (string-length (car input))))))
      (else (helper (nth-cdr input dx) dx dy (remainder (+ y dy) (string-length (car input)))))))
  (helper input dx dy 0))

(define (nth-cdr lst n)
  (if (or (null? lst) (zero? n))
      lst
      (nth-cdr (cdr lst) (- n 1))))

(define (day03-part1 input)
  (run-slope input 1 3))

(define (day03-part2 input)
  (let ((slopes '((1 1) (1 3) (1 5) (1 7) (2 1))))
   (apply * (map (lambda (x) (run-slope input (car x) (cadr x))) slopes))))

(define input (str-split (file->string (open-input-file "input.txt")) "\n"))

(display "PART ONE: ")(display (day03-part1 input))(newline)
(display "PART TWO: ")(display (day03-part2 input))(newline)
