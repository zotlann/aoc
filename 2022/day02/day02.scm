(load "../../lib/lib.scm")

(define (play-game play)
  (let ((opponent (car play))
        (player (cadr play)))
    (cond
      ((string=? opponent "A")
       (cond
         ((string=? player "X") 3)
         ((string=? player "Y") 4)
         ((string=? player "Z") 8)))
      ((string=? opponent "B")
       (cond
         ((string=? player "X") 1)
         ((string=? player "Y") 5)
         ((string=? player "Z") 9)))
      ((string=? opponent "C")
       (cond
         ((string=? player "X") 2)
         ((string=? player "Y") 6)
         ((string=? player "Z") 7))))))

(define input (file->string (open-input-file "input.txt")))
(define input (str-split input "\n"))
(define input (map (lambda (x) (str-split x " ")) input))

(apply +  (map play-game input))
