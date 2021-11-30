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

(define (format-operation op)
  (let ((opcode (string->symbol (car op)))
        (arg (cadr op)))
    (if (char=? (string-ref arg 0) #\+)
        (list opcode (string->number (substring arg 1 (string-length arg))) #f)
        (list opcode (string->number arg) #f)))) 

(define input (str-split (file->string (open-input-file "input.txt")) "\n"))
(define input (map (lambda (x) (str-split x " ")) input))
(define input (map format-operation input))
(define input (list->vector input))

(define (opcode operation) (car operation))
(define (argument operation) (cadr operation))
(define (executed? operation) (caddr operation))

(define (make-operation opcode arg executed?) (list opcode arg executed?))

(define (run-computer-part1 input current-instruction acc) 
  (let ((op (vector-ref input current-instruction)))
   (if (executed? op)
       acc
       (begin
         (vector-set! input current-instruction (make-operation (opcode op) (argument op) #t))
         (cond
           ((eq? (opcode op) 'nop) (run-computer-part1 input (+ current-instruction 1) acc))
           ((eq? (opcode op) 'acc) (run-computer-part1 input (+ current-instruction 1) (+ acc (argument op))))
           ((eq? (opcode op) 'jmp) (run-computer-part1 input (+ current-instruction (argument op)) acc)))))))

(define (run-computer-part2 input current-instruction acc) 
  (if (>= current-instruction (vector-length input))
      acc
      (let ((op (vector-ref input current-instruction)))
       (if (executed? op)
           #f
           (begin
             (vector-set! input current-instruction (make-operation (opcode op) (argument op) #t))
             (cond
               ((eq? (opcode op) 'nop) (run-computer-part2 input (+ current-instruction 1) acc))
               ((eq? (opcode op) 'acc) (run-computer-part2 input (+ current-instruction 1) (+ acc (argument op))))
               ((eq? (opcode op) 'jmp) (run-computer-part2 input (+ current-instruction (argument op)) acc))))))))

(define (day08-part1 input) (run-computer-part1 (vector-copy input) 0 0))

(define (day08-part2 input current-replacement)
  (let ((op (vector-ref input current-replacement)))
   (if (eq? (opcode op) 'acc)
       (day08-part2 input (+ current-replacement 1))
       (let ((new-op (if (eq? (opcode op) 'jmp)
                         (make-operation 'nop (argument op) #f)
                         (make-operation 'jmp (argument op) #f)))
              (new-input (vector-copy input)))
         (vector-set! new-input current-replacement new-op)
         (let ((ret (run-computer-part2 new-input 0 0)))
          (if ret
              ret
              (day08-part2 input (+ current-replacement 1))))))))

(display "PART ONE: ")(display (day08-part1 input))(newline)
(display "PART TWO: ")(display (day08-part2 input 0))(newline)
