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
    ((eq? (car lst) tar) (remove tar (cdr lst)))
    (else (cons (car lst) (remove tar (cdr lst))))))

(define (make-bag-val str)
  (cons (substring str 1 (string-length str)) (string->number (substring str 0 1))))

(define (format-rule rule)
  (let* ((x (str-split rule "bags"))
         (y (apply append (map (lambda (x) (str-split x "bag")) x)))
         (z (apply append (map (lambda (x) (str-split x ", ")) y)))
         (a (apply append (map (lambda (x) (str-split x "contain")) z)))
         (b (map string->list a))
         (c (map (lambda (x) (remove #\space x)) b))
         (d (map list->string (remove '() c))))
    (cons (car d) (map make-bag-val (cdr d)))))

(define rules (map format-rule (str-split (file->string (open-input-file "input.txt")) "\n")))

(define (has-gold? bag rules)
  (let ((sub-bags (cdr (assoc bag rules))))
   (cond
     ((assoc "oother" sub-bags) #f)
     ((assoc "shinygold" sub-bags) #t)
     (else (member? #t (map (lambda (x) (has-gold? x rules)) (map car sub-bags)))))))

(define (member? tar lst)
  (cond
    ((null? lst) #f)
    ((eq? (car lst) tar) #t)
    (else (member? tar (cdr lst)))))

(define (day07-part1 rules)
  (define (helper input rules)
    (cond
      ((null? input) 0)
      ((has-gold? (car input) rules) (+ 1 (helper (cdr input) rules)))
      (else (helper (cdr input) rules))))
  (helper (map car rules) rules))

(define (count-bags bag rules)
  (let ((sub-bags (cdr (assoc bag rules))))
   (if (assoc "oother" sub-bags)
       1
       (+ 1 (apply + (map (lambda (x) (* (cdr x) (count-bags (car x) rules))) sub-bags))))))

(define (day07-part2 rules)
  (- (count-bags "shinygold" rules) 1))

(display "PART ONE: ")(display (day07-part1 rules))(newline)
(display "PART TWO: ")(display (day07-part2 rules))(newline)
