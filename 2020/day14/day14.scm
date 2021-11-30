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

(define (remove tar lst)
  (cond
    ((null? lst) '())
    ((equal? (car lst) tar) (remove tar (cdr lst)))
    (else (cons (car lst) (remove tar (cdr lst))))))

(define input (map (lambda (x) (str-split x " ")) (str-split (file->string (open-input-file "input.txt")) "\n")))
(define input (map (lambda (x) (cons (car x) (caddr x))) input))

(define (string->binary str)
  (let* ((ret (number->string (string->number str) 2))
        (n (- 36 (string-length ret))))
    (string-append (make-string n #\0) ret)))

(define (apply-mask mask str)
  (define (helper m-lst s-lst)
    (cond
      ((null? m-lst) '())
      ((equal? (car m-lst) #\X) (cons (car s-lst) (helper (cdr m-lst) (cdr s-lst))))
      (else (cons (car m-lst) (helper (cdr m-lst) (cdr s-lst))))))
  (let ((mask-lst (string->list mask))
        (str-lst (string->list str)))
    (list->string (helper mask-lst str-lst))))

(define (assoc-replace key val alist)
  (cons (cons key val) (remove-pred (lambda (x) (equal? (car x) key)) alist)))

(define (remove-pred pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst)) (remove-pred pred (cdr lst)))
    (else (cons (car lst) (remove-pred pred (cdr lst))))))

(define (day14-part1 input)
  (define (helper input mask memory)
    (cond
      ((null? input) (sum-memory memory))
      ((equal? (caar input) "mask") (helper (cdr input) (cdar input) memory))
      (else
        (let ((address (get-address (caar input)))
              (val (string->binary (cdar input))))
          (helper (cdr input) mask (assoc-replace address (string->number (apply-mask mask val) 2) memory))))))
  (helper input "" '()))

(define (sum-memory memory) (apply + (map cdr memory)))

(define (apply-mask-pt2 mask str)
  (define (helper m-lst s-lst)
    (cond
      ((null? m-lst) '())
      ((equal? (car m-lst) #\0) (cons (car s-lst) (helper (cdr m-lst) (cdr s-lst))))
      (else (cons (car m-lst) (helper (cdr m-lst) (cdr s-lst))))))
  (list->string (helper (string->list mask) (string->list str))))

(define (get-address str)
  (string->number (substring str 4 (- (string-length str) 1))))

(define (get-floating-address-list lst)
  (define (helper lst acc)
    (cond
      ((null? lst) (cons acc '()))
      ((equal? (car lst) #\X) (cons (helper (cdr lst) (cons #\1 acc))
                                    (helper (cdr lst) (cons #\0 acc))))
      (else (helper (cdr lst) (cons (car lst) acc)))))
  (map (lambda (x) (string->number (list->string (reverse x)) 2)) (flatten (helper lst '()))))

(define (flatten lst)
  (cond
    ((null? lst) '())
    ((pair? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
    (else (list lst))))

(define address (get-floating-address-list (string->list "0X1101X")))

(define (update-addresses addresses val memory)
  (if (null? addresses)
      memory
      (update-addresses (cdr addresses) val (assoc-replace (car addresses) val memory))))

(define (day14-part2 input)
  (define (helper input mask memory)
    (cond
      ((null? input) (sum-memory memory))
      ((equal? (caar input) "mask") (helper (cdr input) (cdar input) memory))
      (else
        (let ((addresses (get-floating-address-list (string->list (apply-mask-pt2 mask (string->binary (number->string (get-address (caar input))))))))
              (val (string->number (cdar input))))
          (helper (cdr input) mask (update-addresses addresses val memory))))))
  (helper (cdr input) (cdar input) '()))

(display (day14-part2 input))(newline)
