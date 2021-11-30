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

(define (parse-line str)
  (let* ((line (str-split str " "))
         (rule (car line))
         (split (str-split rule "-"))
         (lower (string->number (car split)))
         (upper (string->number (cadr split)))
         (char (string-ref (cadr line) 0))
         (pass (caddr line)))
    (list lower upper char pass)))

(define (count tar lst)
  (cond
    ((null? lst) 0)
    ((eq? (car lst) tar) (+ 1 (count tar (cdr lst))))
    (else (count tar (cdr lst)))))

(define (day02-part1 input)
  (cond
    ((null? input) 0)
    ((day02-part1-auth (car input)) (+ 1 (day02-part1 (cdr input))))
    (else (day02-part1 (cdr input)))))

(define (day02-part1-auth str)
  (let* ((line (parse-line str))
         (lower (car line))
         (upper (cadr line))
         (char  (caddr line))
         (pass  (cadddr line))
         (occur (count char (string->list pass))))
    (if (and (<= occur upper) (>= occur lower))
        #t
        #f)))

(define (day02-part2 input)
  (cond
    ((null? input) 0)
    ((day02-part2-auth (car input)) (+ 1 (day02-part2 (cdr input))))
    (else (day02-part2 (cdr input)))))

(define (day02-part2-auth str)
  (let* ((line (parse-line str))
         (lower (car line))
         (upper (cadr line))
         (char  (caddr line))
         (pass  (cadddr line)))
    (if (xor (char=? char (string-ref pass (- lower 1)))
             (char=? char (string-ref pass (- upper 1))))
        #t
        #f)))

(define input (str-split (file->string (open-input-file "input.txt")) "\n"))

(define (xor a b)
  (not (boolean=? a b)))

(define boolean=? eq?)

(display "PART ONE: ")(display (day02-part1 input))(newline)
(display "PART TWO: ")(display (day02-part2 input))(newline)
