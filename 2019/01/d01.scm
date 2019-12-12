;Day01 of 2019 Advent of Code

;finds the fuel cost of a module of a given mass
;the calculation for part 1 is given ass floor(mass/3)-2
(define fuel-cost-part1
  (lambda (mass)
    (- (floor (/ mass 3)) 2)))

;finds the fuel cost of a module given a mass
;the calculation for part 2 is done by recursively applying part 1's mass
;calculation and summing the results, until the resulting cost is negatve
(define fuel-cost-part2
  (lambda (mass)
    (let ((cost (fuel-cost-part1 mass)))
     (if (<= cost 0)
         0
         (+ cost (fuel-cost-part2 cost))))))

;sums up all of the numbers in a list
(define sum
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum (cdr lst))))))

;reads the input file in and returns a list of numbers
(define read-input
  (lambda (filename)
    (define helper
      (lambda (fileport)
        (let ((x (read fileport)))
         (if (eof-object? x)
             '()
             (cons x (helper fileport))))))
    (helper (open-input-file filename))))


(define day01-part1
  (lambda (filename)
    (let ((masses (read-input filename)))
     (sum (map fuel-cost-part1 masses)))))

(define day01-part2
  (lambda (filename)
    (let ((masses (read-input filename)))
     (sum (map fuel-cost-part2 masses)))))

(day01-part2 "input.txt")
