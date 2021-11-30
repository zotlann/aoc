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

(define input (list 1 3 2))

(define (get-index tar lst)
  (cond
    ((null? lst) -100000)
    ((equal? (car lst) tar) 1)
    (else (+ 1 (get-index tar (cdr lst))))))

(define (init-ht input)
  (define (helper ht input)
    (if (null? input)
        ht
        (begin
          (put-hash-table! ht (car input) (- (length input) 1))
          (helper ht (cdr input)))))
  (helper (make-hash-table) input))

(define (increment-hash-table ht)
  (define (helper keys)
  (if (not (null? keys))
      (let ((val (get-hash-table ht (car keys) #f)))
       (if val
           (begin 
             (put-hash-table! ht (car keys) (+ 1 val))
             (helper (cdr keys)))))))
  (helper (hash-table-map ht (lambda (x y) x))))

(define (day14 input n)
  (define (helper ht n current last-said)
    (if (>= current n)
        last-said
        (let ((new (get-hash-table ht last-said #f)))
         (increment-hash-table ht)
         ;(display last-said)(newline)(display new)(newline)(display-ht ht)(newline)
         (if (equal? new #f)
             (helper ht n (+ current 1) 0)
             (begin
               (put-hash-table! ht last-said 1)
               (helper ht n (+ current 1) new))))))
  (helper (init-ht input) n 0 (car (reverse input))))

(define ht (init-ht input))
(define (display-ht ht) (display (hash-table-map ht (lambda (x y) (cons x y)))))

(day14 input 5)
