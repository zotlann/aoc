(load "../../lib/lib.scm")

(define *forward* "forward")
(define *up* "up")
(define *down* "down")

(define parse-direction (lambda (x) x))
(define input (map (lambda (x) (str-split x " ")) (str-split (file->string (open-input-file "input.txt")) "\n")))
(define input (map (lambda (x) (list (car x) (string->number (cadr x)))) input))

(define (command direction distance) (list direction distance))
(define (get-direction command) (car command))
(define (get-distance command) (cadr command))

(define (make-position x y) (list x y))
(define (get-x position) (car position))
(define (get-y position) (cadr position))

(define (make-submarine position aim) (list position aim))
(define (get-position submarine) (car submarine))
(define (get-aim submarine) (cadr submarine)) 

(define (move-position command position)
  (let ((direction (get-direction command))
        (distance (get-distance command)))
    (cond
      ((equal? *forward* direction) (make-position (+ distance (get-x position)) (get-y position)))
      ((equal? *down* direction) (make-position (get-x position) (+ distance (get-y position))))
      ((equal? *up* direction) (make-position (get-x position) (- (get-y position) distance))))))

(define (move-position-part2 command submarine)
  (let ((direction (get-direction command))
        (distance (get-distance command))
        (position (get-position submarine))
        (aim (get-aim submarine)))
    (cond
      ((equal? *forward* direction) (make-submarine (make-position (+ distance (get-x position)) (+ (get-y position) (* aim distance))) aim))
      ((equal? *down* direction) (make-submarine position (+ aim distance)))
      ((equal? *up* direction) (make-submarine position (- aim distance))))))

(define (day02-part1 input)
  (define (helper input position)
    (cond
      ((null? input) (* (get-x position) (get-y position)))
      (else (helper (cdr input) (move-position (car input) position)))))
  (helper input (make-position 0 0)))

(define (day02-part2 input)
  (define (helper input submarine)
    (cond
      ((null? input) (* (get-x (get-position submarine)) (get-y (get-position submarine))))
      (else (helper (cdr input) (move-position-part2 (car input) submarine)))))
  (helper input (make-submarine (make-position 0 0) 0)))

(display (day02-part2 input))
