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

(define (set-union s1 s2)
  (cond
    ((null? s1) s2)
    ((member (car s1) s2) (set-union (cdr s1) s2))
    (else (cons (car s1) (set-union (cdr s1) s2)))))

(define (set-intersection s1 s2)
  (cond
    ((null? s1) '())
    ((member (car s1) s2) (cons (car s1) (set-intersection (cdr s1) s2)))
    (else (set-intersection (cdr s1) s2))))

(define (combine-answers-pt2 l)
  (define (helper l acc)
    (if (null? l)
        acc
        (helper (cdr l) (set-intersection acc (car l)))))
  (let ((l (map string->list l)))
   (helper (cdr l) (car l))))

(define (combine-answers-pt1 l)
  (define (helper l acc)
    (if (null? l)
        acc
        (helper (cdr l) (set-union acc (car l)))))
  (helper (map string->list l) '()))

(define (list-sum l)
  (cond
    ((null? l) 0)
    (else (+ (car l) (list-sum (cdr l))))))

(define (day06-part1 input)
  (list-sum (map length (map combine-answers-pt1 input))))

(define (day06-part2 input)
  (list-sum (map length (map combine-answers-pt2 input))))

(define input (map (lambda (x) (str-split x "\n")) (str-split (file->string (open-input-file "input.txt")) "\n\n")))
(display "PART ONE: ")(display (day06-part1 input))(newline)
(display "PART TWO: ")(display (day06-part2 input))(newline)
