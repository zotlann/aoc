;runs the computer recursively until the halt code is reached
;when the computer halts, returns the value in the 0th memory address
(define run-computer
  (lambda (memory)
    (define helper
      (lambda (ip memory)
        (if (equal? 'halt (get-operation (dereference memory ip)))
            (dereference memory 0)
            (let ((opcode (dereference memory ip))
                  (rand1 (dereference memory (dereference memory (+ ip 1))))
                  (rand2 (dereference memory (dereference memory (+ ip 2))))
                  (result-location (dereference memory (+ ip 3))))
              (write-memory memory result-location (apply-operation opcode rand1 rand2))
              (helper (+ ip 4) memory)))))
    (helper 0 memory)))

;get the value at a given memory address
(define dereference
  (lambda (memory address)
    (vector-ref memory address)))

;write a value to a given memory address
(define write-memory
  (lambda (memory address value)
    (vector-set! memory address value)))

;apply an opcode to two operands and return the value
(define apply-operation
  (lambda (opcode rand1 rand2)
    ((get-operation opcode) rand1 rand2)))

;looks up the opcode and returns the operation it performs
(define get-operation
  (lambda (opcode)
    (cond
      ((eq? 1 opcode) (lambda (x y) (+ x y)))
      ((eq? 2 opcode) (lambda (x y) (* x y)))
      ((eq? 99 opcode) 'halt))))

;reads in the input file and returns a vector of numbers represeting the computer memory
(define read-input
  (lambda (filename)
    (define helper
      (lambda (fileport)
        (let ((x (peek-char fileport)))
         (if (equal? #\, x)
             (begin (read-char fileport) (helper fileport))
             (let ((x (read fileport)))
              (if (eof-object? x)
                  '()
                  (cons x (helper fileport))))))))
    (list->vector (helper (open-input-file filename)))))

(define copy-memory
  (lambda (memory)
    (vector-copy memory)))

       
;read in the initial memory state and then run the computer until it halts
;replaces the value at address 1 with 12 and the value in address 2 with 2
;as per part1 instructions
(define day02-part1
  (lambda (filename)
    (let ((memory (read-input filename)))
     (write-memory memory 1 12)
     (write-memory memory 2 2)
     (run-computer memory))))

(define test-nouns
  (lambda (memory noun-min noun-max verb-min verb-max desired-output)
    (define helper
      (lambda (memory current-noun noun-max verb-min verb-max desired-output)
        (if (> current-noun noun-max)
            #f
            (let ((noun-verb (test-verbs memory current-noun verb-min verb-max desired-output)))
             (if noun-verb
                 noun-verb
                 (helper memory (+ current-noun 1) noun-max verb-min verb-max desired-output))))))
    (helper memory noun-min noun-max verb-min verb-max desired-output)))

(define test-verbs
  (lambda (memory noun verb-min verb-max desired-output)
    (define helper
      (lambda (memory noun current-verb verb-max desired-output)
        (if (> current-verb verb-max)
            #f
            (let ((noun-verb (test-noun-verb memory noun current-verb desired-output)))
             (if noun-verb
                 noun-verb
                 (helper memory noun (+ current-verb 1) verb-max desired-output))))))
    (helper memory noun verb-min verb-max desired-output)))

(define test-noun-verb
  (lambda (memory noun verb desired-output)
    (let ((mem-copy (copy-memory memory)))
     (write-memory mem-copy 1 noun)
     (write-memory mem-copy 2 verb)
     (if (= (run-computer mem-copy) desired-output)
         (cons noun verb)
         #f))))

(define day02-part2
  (lambda (filename)
    (let ((memory (read-input filename))
          (noun-min 0)
          (noun-max 99)
          (verb-min 0)
          (verb-max 99)
          (desired-output 19690720))
      (test-nouns memory noun-min noun-max verb-min verb-max desired-output))))

(day02-part2 "input.txt")
