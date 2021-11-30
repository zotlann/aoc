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

(define (make-range lower upper) (cons lower upper))
(define (range-lower range) (car range))
(define (range-upper range) (cdr range))

(define (make-field name lower-range upper-range)
  (list name lower-range upper-range))
(define (field-name field) (car field))
(define (field-lower field) (cadr field))
(define (field-upper field) (caddr field))

(define (in-range? val range)
  (let ((lower (range-lower range))
        (upper (range-upper range)))
    (and (<= val upper) (>= val lower))))

(define (valid-field? val field)
  (let ((range-lower (field-lower field))
        (range-upper (field-upper field)))
    (or (in-range? val range-lower) (in-range? val range-upper))))

(define (any pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) #t)
    (else (any pred (cdr lst)))))

(define (all pred lst)
  (cond
    ((null? lst) #t)
    ((pred (car lst)) (all pred (cdr lst)))
    (else #f)))

(define (parse-field raw-str)
  (let* ((s1 (str-split raw-str ":"))
         (name (car s1))
         (range-s (str-split (cadr s1) " "))
         (r1 (map string->number (str-split (car range-s) "-")))
         (r2 (map string->number (str-split (caddr range-s) "-"))))
    (make-field name (make-range (car r1) (cadr r1)) (make-range (car r2) (cadr r2)))))

(define input-file (open-input-file "input.txt"))
(define raw-input (str-split (file->string input-file) "\n\n"))
(define raw-fields (car raw-input))
(define raw-my-ticket (cadr raw-input))
(define raw-nearby-tickets (caddr raw-input))
(define test (car (str-split raw-fields "\n")))
(define my-ticket (map string->number (str-split (cadr (str-split raw-my-ticket "\n")) ",")))

(define fields (map parse-field (str-split raw-fields "\n")))
(define tickets (map (lambda (x) (map string->number x)) (map (lambda (x) (str-split x ",")) (cdr (str-split raw-nearby-tickets "\n")))))

(define (day16-part1 fields tickets)
  (if (null? tickets)
      0
      (+ (error-rate (car tickets) fields) (day16-part1 fields (cdr tickets)))))

(define (error-rate ticket fields)
  (cond
    ((null? ticket) 0)
    ((any (lambda (x) (valid-field? (car ticket) x)) fields) (error-rate (cdr ticket) fields))
    (else (+ (car ticket) (error-rate (cdr ticket) fields)))))

(day16-part1 fields tickets)

(define (format-tickets tickets)
  (let loop ((lst tickets) (n 0))
   (if (null? (car lst))
       '()
       (cons (cons (map car tickets) n) (loop (map cdr lst) (+ n 1))))))

(define (cull-fields fields)
  (cond
    ((null? fields) '())
    ((equal? "departure" (car (str-split (field-name (car fields)) " ")))
     (cons (car fields) (cull-fields (cdr fields))))
    (else (cull-fields (cdr fields)))))

(define fields-part2 (cull-fields fields))

(define (day16-part2 fields tickets my-ticket)
  (if (null? tickets)
      '()
      (let ((valid-fields (filter (lambda (x) (all (lambda (y) (valid-field? y x)) (caar tickets))) fields)))
       (if (null? valid-fields)
           (day16-part2 fields (cdr tickets) my-ticket)
           (let ((n (cdr (car tickets))))
            (cons (list-ref my-ticket n) (day16-part2 fields (cdr tickets) my-ticket)))))))
(define tickets-part2 (format-tickets (filter (lambda (x) (zero? (error-rate x fields))) (append (list my-ticket) tickets))))

(define (n-applicable field tickets)
  (cond 
    ((null? tickets) 0)
    ((all (lambda (x) (valid-field? x field)) (map car tickets))
     (+ 1 (n-applicable field (cdr tickets))))
    (else (n-applicable field (cdr tickets)))))

(map (lambda (x) (n-applicable x (map car tickets-part2))) fields)
