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

(define (parse-input input)
  (let* ((file-string (file->string (open-input-file input)))
         (s1 (str-split file-string "\n\n"))
         (s2 (map (lambda (x) (str-split x "\n")) s1))
         (s3 (map (lambda (y) (map (lambda (x) (str-split x " ")) y)) s2))
         (s4 (map (lambda (x) (apply append x)) s3)))
    (map (lambda (x) (map make-kv-pair x)) s4)))



(define (make-kv-pair str)
  (let* ((s (str-split str ":"))
         (key (car s))
         (val (cadr s)))
    `(,key . ,val)))

(define (day04-part1 input)
  (cond
    ((null? input) 0)
    ((part1-auth (car input)) (+ 1 (day04-part1 (cdr input))))
    (else (day04-part1 (cdr input)))))

(define (part1-auth alist)
  (and (assoc "byr" alist) (assoc "iyr" alist) (assoc "eyr" alist) (assoc "hgt" alist) (assoc "hcl" alist) (assoc "ecl" alist) (assoc "pid" alist)))

(define (day04-part2 input)
  (cond
    ((null? input) 0)
    ((part2-auth (car input)) (+ 1 (day04-part2 (cdr input))))
    (else (day04-part2 (cdr input)))))

(define (part2-auth alist)
  (and (v-byr alist) (v-iyr alist) (v-eyr alist) (v-hgt alist) (v-hcl alist) (v-ecl alist) (v-pid alist)))

(define (v-byr alist)
  (let ((byr (assoc "byr" alist)))
   (and byr (= 4 (string-length (cdr byr))) (>= 2002 (string->number (cdr byr))) (<= 1920 (string->number (cdr byr))))))

(define (v-iyr alist)
  (let ((iyr (assoc "iyr" alist)))
   (and iyr (= 4 (string-length (cdr iyr))) (>= 2020 (string->number (cdr iyr))) (<= 2010 (string->number (cdr iyr))))))

(define (v-eyr alist)
  (let ((eyr (assoc "eyr" alist)))
   (and eyr (= 4 (string-length (cdr eyr))) (>= 2030 (string->number (cdr eyr))) (<= 2020 (string->number (cdr eyr))))))

(define (v-hgt alist)
  (let ((hgt (assoc "hgt" alist)))
   (and hgt (validate-height (cdr hgt)))))

(define (validate-height str)
  (if (< (string-length str) 4) #f
  (let ((c1 (string-ref str 0))
        (c2 (string-ref str 1))
        (c3 (string-ref str 2))
        (c4 (string-ref str 3)))
    (cond
      ((char=? c3 #\i)
       (let ((n (string->number (list->string (list c1 c2)))))
        (and (>= n 59) (<= n 76))))
      ((char=? c4 #\c)
       (let ((n (string->number (list->string (list c1 c2 c3)))))
        (and (>= n 150) (<= n 193))))
      (else #f)))))

(define (v-hcl alist)
  (let ((hcl (assoc "hcl" alist)))
   (and hcl (validate-hcl (cdr hcl)))))

(define (validate-hcl str)
  (and (= (string-length str) 7) (char=? #\# (string-ref str 0)) (hcl-help (string->list str))))

(define (hcl-help l)
  (if (null? l) 
      #t
      (let ((n (char->integer (car l))))
       (and (or (char=? #\# (car l)) (and (>= n 48) (<= n 57)) (and (>= n 97) (<= n 102)))
            (hcl-help (cdr l))))))

(define (v-ecl alist)
  (let ((ecl (assoc "ecl" alist)))
   (and ecl (validate-ecl (cdr ecl)))))

(define (validate-ecl str)
  (or (string=? "amb" str)(string=? "blu" str)(string=? "brn" str)(string=? "gry" str)(string=? "grn" str)(string=? "hzl" str)(string=? "oth" str)))

(define (v-pid alist)
  (let ((pid (assoc "pid" alist)))
   (and pid (= 9 (string-length (cdr pid))))))

(define input (parse-input "input.txt"))
(display "PART ONE: ")(display (day04-part1 input))(newline)
(display "PART TWO: ")(display (day04-part2 input))(newline)
