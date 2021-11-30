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

(define (score str)
  (define (helper lst)
    (cond
      ((null? lst) '())
      ((or (char=? (car lst) #\F) (char=? (car lst) #\L)) (cons #\0 (helper (cdr lst))))
      (else (cons #\1 (helper (cdr lst))))))
  (string->number (list->string (helper (string->list str))) 2))

(define (max-list lst)
  (define (helper lst curr-max)
    (cond
      ((null? lst) curr-max)
      ((>= (car lst) curr-max) (helper (cdr lst) (car lst)))
      (else (helper (cdr lst) curr-max))))
  (helper lst (car lst)))

(define (find-missing lst)
  (cond
    ((or (null? lst) (null? (cdr lst))) #f)
    ((= (+ (car lst) 1) (cadr lst)) (find-missing (cdr lst)))
    (else (+ (car lst) 1))))

(define input (str-split (file->string (open-input-file "input.txt")) "\n")) 

(define (day05-part1 input)
  (max-list (map score input)))
(define (day05-part2 input)
  (find-missing (sort < (map score input))))

(define input (str-split (file->string (open-input-file "input.txt")) "\n"))

(display "PART ONE: ")(display (day05-part1 input))(newline)
(display "PART TWO: ")(display (day05-part2 input))(newline)
