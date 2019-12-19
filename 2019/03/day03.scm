;a point is a pair of coordinates
(define make-point
  (lambda (x y)
    (cons x y)))
(define get-x
  (lambda (point)
    (car point)))
(define get-y
  (lambda (point)
    (cdr point)))

;the manhattan distance between two points (x1,y1) (x2,y2)
;is given by abs(x2-x1) + abs(y2-y1)
(define manhattan-distance
  (lambda (p1 p2)
    (+ (abs (- (get-x p2) (get-x p1))) (abs (- (get-y p2) (get-y p1))))))

;definition of square because it wasn't defined in chicken
(define square
  (lambda (x)
    (* x x)))

;the straight-line distance between two points (x1,y1) (x2,y2)
;is given by sqrt((x2-x1)^2 + (y2-y1)^2)
(define distance
  (lambda (p1 p2)
    (sqrt (+ (square (- (get-x p2) (get-x p1)))  (square (- (get-y p2) (get-y p1)))))))

;a line segment is defined using a pair of points
(define make-line
  (lambda (a b)
    (cons a b)))
(define get-a
  (lambda (line)
    (car line)))
(define get-b
  (lambda (line)
    (cdr line)))

;A point is on a line if the distance between it and one point defining the line plus the distance
;between it and the other point defining the line is equal to the length of the line
(define on-line?
  (lambda (point line)
    (equal? (+ (distance (get-a line) point) (distance (get-b line) point)) (line-length line))))

;The length of a line is the distance between the two points defining it
(define line-length
  (lambda (line)
    (distance (get-a line) (get-b line))))

;returns the point at which two lines intersect if they do, #f otherwise
;Uses LeMothe's algorithm taken from https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect 
(define get-intersection
  (lambda (l1 l2)
    (let* ((p0 (get-a l1))
           (p1 (get-b l1))
           (p2 (get-a l2))
           (p3 (get-b l2))
           (s1 (make-line (- (get-x p1) (get-x p0)) (- (get-y p1) (get-y p0))))
           (s2 (make-line (- (get-x p3) (get-x p2)) (- (get-y p3) (get-y p2))))
           (snumerator (+ (* -1 (get-y s1) (- (get-x p0) (get-x p2))) (* (get-x s1) (- (get-y p0) (get-y p2)))))
           (tnumerator (- (* (get-x s2) (- (get-y p0) (get-y p2))) (* (get-y s2) (- (get-x p0) (get-x p2)))))
           (determinant (+ (* -1 (get-x s2) (get-y s1)) (* (get-x s1) (get-y s2)))))
      (if (= determinant 0)
          #f
          (let ((s (/ snumerator determinant))
                (t (/ tnumerator determinant)))
            (if (and (>= s 0) (<= s 1) (>= t 0) (<= t 1))
                (make-point (+ (get-x p0) (* t (get-x s1))) (+ (get-y p0) (* t (get-y s1))))
                #f))))))

;a wire is a list of line segments created by following movement instructions
;each instruction is a symbol whose first character is a letter LRUD, indicating direction
;left,right,up,down respectively.  The rest of the symbol is a number indicating the distance
;to move in that direction
(define get-direction
  (lambda (instruction)
    (string-ref (symbol->string instruction) 0)))
(define get-distance
  (lambda (instruction)
    (string->number (list->string (cdr (string->list (symbol->string instruction)))))))

;some versions of scheme return #\l for (get-direction LXX) while otherse return #\L
;these functions are used for portability
(define right?
  (lambda (direction)
    (or (equal? #\r direction) (equal? #\R direction))))
(define left?
  (lambda (direction)
    (or (equal? #\l direction) (equal? #\L direction))))
(define up?
  (lambda (direction)
    (or (equal? #\u direction) (equal? #\U direction))))
(define down?
  (lambda (direction)
    (or (equal? #\d direction) (equal? #\D direction))))

;make a wire from a list of instructions in the form DN
;where D is LRUD for left right up and down, and N is the distance
;(e.g) U25 for up 25 or L5 for left 5
(define make-wire
  (lambda (instructions)
    (define helper
      (lambda (instructions previous-point)
        (if (null? instructions)
            '()
            (let
             ((instruction (car instructions))
              (direction (get-direction (car instructions)))
              (distance (get-distance (car instructions))))
             (cons (make-line previous-point (get-new-point previous-point direction distance))
                   (helper (cdr instructions) (get-new-point previous-point direction distance)))))))
    (helper instructions (make-point 0 0))))
(define get-new-point
  (lambda (previous-point direction distance)
    (cond
      ((left? direction) (make-point (- (get-x previous-point) distance) (get-y previous-point)))
      ((right? direction) (make-point (+ (get-x previous-point) distance) (get-y previous-point)))
      ((up? direction) (make-point (get-x previous-point) (+ (get-y previous-point) distance)))
      ((down? direction) (make-point (get-x previous-point) (- (get-y previous-point) distance))))))

;removes all instances of tar from lst
(define remove
  (lambda (tar lst)
    (cond
      [(null? lst) '()]
      [(equal? (car lst) tar) (remove tar (cdr lst))]
      (else (cons (car lst) (remove tar (cdr lst)))))))

;returns a list of all points in which the two wires intersect
(define get-wire-intersections
  (lambda (w1 w2)
    (remove (make-point 0 0) (apply append (map (lambda (x) (get-line-wire-intersections x w2)) w1)))))

;returns a list of all points in which the line intersects the wire
(define get-line-wire-intersections
  (lambda (line wire)
    (remove #f (map (lambda (x) (get-intersection line x)) wire))))

;find the distance of wire used to get to point
(define wire-distance
  (lambda (wire point)
    (cond 
      [(null? wire) 0]
      [(equal? (get-a (car wire)) point) 0]
      [(on-line? point (car wire))
       (+ (manhattan-distance (get-a (car wire)) point))]
      (else (+ (manhattan-distance (get-a (car wire)) (get-b (car wire))) (wire-distance (cdr wire) point))))))

;find the sum of distances that both wires must use to get to point
(define sum-of-wire-distances
  (lambda (w1 w2 point)
    (+ (wire-distance w1 point) (wire-distance w2 point))))


;returns the smallest number in a list
(define min-list
  (lambda (lst)
    (define helper
      (lambda (lst min-so-far)
        (cond
          [(null? lst) min-so-far]
          [(< (car lst) min-so-far) (helper (cdr lst) (car lst))]
          (else (helper (cdr lst) min-so-far)))))
    (helper (cdr lst) (car lst))))

;returns a pair of lists of instructions for making wires
;the input file should be in the form of the example input.txt
;this is just copy-pasted from the advent of code input
(define read-input
  (lambda (filename)
    (define helper
      (lambda (fileport w1-instructions w2-instructions w1-done?)
        (let ((x (peek-char fileport)))
         (cond
           [(equal? #\, x) (begin
                             (read-char fileport)
                             (helper fileport w1-instructions w2-instructions w1-done?))]
           [(equal? #\newline x) (begin
                                   (read-char fileport)
                                   (helper fileport w1-instructions w2-instructions #t))]
           [(eof-object? x) (cons w1-instructions w2-instructions)]
           (else
             (let ((x (read fileport)))
              (if w1-done?
                  (helper fileport w1-instructions (append w2-instructions (list x)) w1-done?)
                  (helper fileport (append w1-instructions (list x)) w2-instructions w1-done?))))))))
    (helper (open-input-file filename) '() '() #f)))

;find the closest intersection between the two wires using the manhattan-distance from the origin
(define day03-part1
  (lambda (filename)
    (let* ((instructions (read-input filename))
          (w1-instructions (car instructions))
          (w2-instructions (cdr instructions))
          (w1 (make-wire w1-instructions))
          (w2 (make-wire w2-instructions)))
      (min-list (map (lambda (x) (manhattan-distance x (make-point 0 0))) (get-wire-intersections w1 w2))))))

;find the closest intersection between the two wires using the distance of wire used until the intersection
(define day03-part2
  (lambda (filename)
    (let* ((instructions (read-input filename))
          (w1-instructions (car instructions))
          (w2-instructions (cdr instructions))
          (w1 (make-wire w1-instructions))
          (w2 (make-wire w2-instructions)))
      (min-list (map (lambda (x) (sum-of-wire-distances w1 w2 x)) (get-wire-intersections w1 w2))))))

