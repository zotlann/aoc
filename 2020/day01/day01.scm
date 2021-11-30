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

(define (day01-part1 input)
  (cond
    ((null? input) #f)
    ((member (- 2020 (car input)) (cdr input)) (* (car input) (- 2020 (car input))))
    (else (day01-part1 (cdr input)))))

(define (day01-part2 input)
  (define (helper n input)
    (cond
      ((null? input) #f)
      ((member (- 2020 n (car input)) (cdr input)) (* n (car input) (- 2020 (car input) n)))
      (else (helper n (cdr input)))))
  
  (if (null? input)
      #f
      (let ((n (helper (car input) (cdr input))))
       (if n
           n
           (day01-part2 (cdr input))))))

(define input (map string->number (str-split (file->string (open-input-file "input.txt")) "\n")))
(display "PART ONE: ")(display (day01-part1 input))(newline)
(display "PART TWO: ")(display (day01-part2 input))(newline)
