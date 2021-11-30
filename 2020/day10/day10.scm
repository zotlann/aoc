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

(define (can-connect? a b) (<= (abs (- a b)) 3))

(define input (sort < (map string->number (str-split (file->string (open-input-file "input.txt")) "\n"))))

(define (day10-part1 input)
  (define (helper input current one-diff three-diff)
    (cond
      ((null? input) (* one-diff three-diff))
      ((= (- (car input) current) 1) (helper (cdr input) (car input) (+ one-diff 1) three-diff))
      ((= (- (car input) current) 3) (helper (cdr input) (car input) one-diff (+ three-diff 1)))
      (else (helper (cdr input) (car input) one-diff three-diff))))
  (helper (cdr input) (car input) 1 1))

(define (day10-part2 input)
  (define (helper input end i a b c)
    (if (or (null? input) (= end i))
        c
        (let ((s (if (member i input) (+ a b c) 0)))
         (helper input end (+ 1 i) b c s))))
  (helper input (+ (car (reverse input)) 1) 1 0 0 1))

(define (replace-counts cnt indices i)
  (if (null? indices)
      cnt
      (let ((new-cnt (replace-index cnt i (+ (list-ref cnt i) (list-ref cnt (car indices))))))
       (replace-counts new-cnt (cdr indices) i))))

(define (nth-cdr lst n)
  (cond
    ((null? lst) '())
    ((zero? n) lst)
    (else (nth-cdr (cdr lst) (- n 1)))))

(define (replace-index lst i new)
  (if (>= i (length lst))
      #f
      (append (take i lst) (list new) (nth-cdr lst (+ i 1)))))

(define (filter-indices pred lst)
  (define (helper pred lst n)
    (cond
      ((null? lst) '())
      ((pred (car lst)) (cons n (helper pred (cdr lst) (+ n 1))))
      (else (helper pred (cdr lst) (+ n 1)))))
  (helper pred lst 0))

(define (take n lst)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))
    

(replace-index '(1 2 3) 3 0)
(display "PART ONE: ")(display (day10-part1 input))(newline)
(display "PART TWO: ")(display (day10-part2 input))(newline)
