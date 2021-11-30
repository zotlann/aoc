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

(define (removep pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst)) (removep pred (cdr lst)))
    (else (cons (car lst) (removep pred (cdr lst))))))

(define departure-time (read (open-input-file "input.txt")))
(define bus-list (str-split (cadr (str-split (file->string (open-input-file "input.txt")) "\n")) ","))

(define (departs? timestamp bus) (zero? (remainder timestamp bus)))

(define (day13-part1 timestamp bus-list)
  (define (helper curr-time bus-list)
    (let ((bus (filter (lambda (x) (departs? curr-time x)) bus-list)))
     (if (null? bus)
         (helper (+ curr-time 1) bus-list)
         (* (car bus) (- curr-time timestamp)))))
  (helper timestamp bus-list))

(day13-part1 departure-time (map string->number (filter string->number bus-list)))

(define (format-input bus-list)
  (define (helper lst n)
    (if (null? lst)
        '()
        (cons (cons (car lst) n) (helper (cdr lst) (+ n 1)))))
  (helper bus-list 0))

(define (any pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) #t)
    (else (any pred (cdr lst)))))

(define (all pred lst)
  (cond
    ((null? lst) #t)
    ((not (pred (car lst))) #f)
    (else (all pred (cdr lst)))))

(define (departs-pt2? timestamp bus/rel-time bus-list)
  (let* ((bus (car bus/rel-time))
        (rel-time (cdr bus/rel-time))
        (time (+ timestamp rel-time)))
    (if (equal? bus "x")
        (any (lambda (x) (departs? time x)) bus-list)
        (departs? time bus))))

(define input (append (sort (lambda (x y) (> (car x) (car y))) (map (lambda (x) (cons (string->number (car x))(cdr x))) (filter (lambda (x) (string->number (car x))) (format-input bus-list)))) (filter (lambda (x) (not (string->number (car x)))) (format-input bus-list))))

;(define input (map (lambda (x) (cons (car x) (- (cdr x) (cdar input)))) input))

(define (day13-part2 buses wildcards)
  (define (helper curr-time buses wildcards step bus-list)
    (display "CURRENT TIME: ")(display curr-time)(newline)
    (cond
      ((and (null? buses) (all (lambda (x) (departs-pt2? curr-time x bus-list)) wildcards)) (- curr-time step))
      ((null? buses) (helper (+ curr-time step) buses wildcards step bus-list))
      ((departs-pt2? curr-time (car buses) bus-list) (helper (+ curr-time (* step (caar buses))) (cdr buses) wildcards (* step (caar buses)) bus-list))
      (else (helper (+ curr-time step) buses wildcards step bus-list))))
  (helper 0 buses wildcards 1 (map car buses)))
    
(define buses (map (lambda (x) (cons (string->number (car x)) (cdr x)))(removep (lambda (x) (equal? (car x) "x")) (format-input bus-list))))
(define wildcards (filter (lambda (x) (equal? (car x) "x")) (format-input bus-list)))

(day13-part2 buses '())
