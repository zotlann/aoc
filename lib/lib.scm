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

(define (member? tar lst)
  (if (member tar lst)
      #t
      #f))

(define (take n lst)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (zero? n) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

(define (set? lst)
  (cond
    ((null? lst) #t)
    ((member? (car lst) (cdr lst)) #f)
    (else (set? (cdr lst)))))

(define (make-set lst)
  (cond
    ((null? lst) '())
    ((member (car lst) (cdr lst)) (make-set (cdr lst)))
    (else (cons (car lst) (make-set (cdr lst))))))

(define (remove-last lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (car lst) (remove-last (cdr lst)))))

(define (rember tar lst)
  (cond
    ((null? lst) '())
    ((equal? tar (car lst)) (rember tar (cdr lst)))
    (else (cons (car lst) (rember tar (cdr lst))))))
