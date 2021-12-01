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


(define input (str-split (file->string (open-input-file "input.txt")) "\n"))
(define test-input (str-split (file->string (open-input-file "test-input.txt")) "\n"))

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
  (let* ((a (car input))
        (b (cadr input))
        (c (caddr input))
        (d (cadddr input))
        (n1 (+ a b c))
        (n2 (+ b c d)))
    (if (> n2 n1) 1 0)))

(day01-part2 (map string->number input))
