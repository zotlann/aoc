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

(define input (map string->number (str-split (file->string (open-input-file "input.txt")) "\n")))
input

(define (has-sum? lst n)
  (cond
    ((null? lst) #f)
    ((member (- n (car lst)) (cdr lst)) #t)
    (else (has-sum? (cdr lst) n))))

(define (take n lst)
  (cond
    ((or (zero? n) (null? lst)) '())
    (else (cons (car lst) (take (- n 1) (cdr lst))))))

(define (day09-part1 input)
  (let ((preamble (take 25 input))
        (n (list-ref input 25)))
    (if (has-sum? preamble n)
        (day09-part1 (cdr input))
        n)))

(define (find-sum lst n)
  (define (helper lst n lower higher acc)
    (if (null? lst)
        #f
        (let* ((new-val (car lst))
               (new-sum (+ new-val acc))
               (new-lower (if (< new-val lower)
                              new-val
                              lower))
               (new-higher (if (> new-val higher)
                               new-val
                               higher)))
          (cond
            ((= n new-sum) (+ new-higher new-lower))
            ((> new-sum n) #f)
            (else (helper (cdr lst) n new-lower new-higher new-sum))))))
  (helper (cdr lst) n (car lst) (car lst) (car lst)))
            

(define (sum-master input n)
  (if (null? input) 
      #f
      (let ((sol (find-sum input n)))
       (if sol
           sol
           (sum-master (cdr input) n)))))

(define (day09-part2 input)
  (let* ((checksum (day09-part1 input))
         (n (sum-master input checksum)))
    n))

(display "PART ONE: ")(display (day09-part1 input))(newline)
(display "PART TWO: ")(display (day09-part2 input))(newline)
