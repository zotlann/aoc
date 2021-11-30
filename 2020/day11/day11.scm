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

(define occupied #\#)
(define empty #\L)
(define floor #\.)

(define (occupied? c) (char=? c occupied))
(define (empty? c) (char=? c empty))
(define (floor? c) (char=? floor c))

(define (apply-rules input x y rule tolerance)
  (let ((n-occupied (rule input x y)))
   (cond
     ((floor? (vector-ref (vector-ref input x) y)) floor)
     ((zero? n-occupied) occupied)
     ((>= n-occupied tolerance) empty)
     (else (vector-ref (vector-ref input x) y)))))

(define (apply-rules-global input rule tolerance)
  (define (helper input x xlen)
    (if (>= x xlen)
        '()
        (cons (apply-rules-row input x rule tolerance) (helper input (+ x 1) xlen))))
  (helper input 0 (vector-length input)))

(define (apply-rules-row input x rule tolerance)
  (define (helper input x y ylen)
    (if (>= y ylen)
        '()
        (cons (apply-rules input x y rule tolerance) (helper input x (+ y 1) ylen))))
  (helper input x 0 (vector-length (vector-ref input 0))))


(define (count tar lst)
  (cond
    ((null? lst) 0)
    ((eq? (car lst) tar) (+ 1 (count tar (cdr lst))))
    (else (count tar (cdr lst)))))

(define (run-sim input rule tolerance)
  (let ((output (list->vector (map list->vector (apply-rules-global input rule tolerance)))))
   (if (equal? input output)
       (apply + (map (lambda (x) (count occupied (vector->list x))) (vector->list output)))
       (run-sim output rule tolerance))))

(define (day11-part1 input) (run-sim input count-occupied-part1 4))
(define (day11-part2 input) (run-sim input count-occupied-part2 5))

(define (count-occupied-part1 input x y)
  (let ((nw (check-seat input (- x 1) (- y 1)))
        (n  (check-seat input (- x 1) y))
        (ne (check-seat input (- x 1) (+ y 1)))
        (e  (check-seat input x (+ y 1)))
        (se (check-seat input (+ x 1) (+ y 1)))
        (s  (check-seat input (+ x 1) y))
        (sw (check-seat input (+ x 1) (- y 1)))
        (w  (check-seat input x (- y 1))))
    (count occupied (list nw n ne e se s sw w))))

(define (check-seat input x y)
  (let ((xlen (vector-length input))
        (ylen (vector-length (vector-ref input 0))))
    (if (or (>= x xlen) (>= y ylen) (negative? x) (negative? y))
        floor
        (vector-ref (vector-ref input x) y))))

(define (check-seats input x y dx dy)
  (let ((new-x (+ x dx))
        (new-y (+ y dy))
        (xlen (vector-length input))
        (ylen (vector-length (vector-ref input 0))))
    (if (or (>= new-x xlen) (>= new-y ylen) (negative? new-x) (negative? new-y))
        floor
        (let ((found (vector-ref (vector-ref input new-x) new-y)))
         (if (floor? found)
             (check-seats input new-x new-y dx dy)
             found)))))

(define (count-occupied-part2 input x y)
  (let ((nw (check-seats input x y -1 -1))
        (n  (check-seats input x y -1 0))
        (ne (check-seats input x y -1 1))
        (e  (check-seats input x y 0 1))
        (se (check-seats input x y 1 1))
        (s  (check-seats input x y 1 0))
        (sw (check-seats input x y 1 -1))
        (w  (check-seats input x y 0 -1)))
    (count occupied (list nw n ne e se s sw w))))

(define input (list->vector (map list->vector (map string->list (str-split (file->string (open-input-file "input.txt")) "\n")))))

(display "PART ONE: ")(display (day11-part1 input))(newline)
(display "PART TWO: ")(display (day11-part2 input))(newline)
