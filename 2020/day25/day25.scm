(require math/number-theory)

(define (transform subject-number loop-size)
  (modular-expt subject-number loop-size 20201227))

(define (find-loop n)
  (define (helper n loop)
    (if (= (transform 7 loop) n)
        loop
        (helper n (+ loop 1))))
  (helper n 1))

(define door-pub 18499292)
(define key-pub 8790390)

(define priv1 (modular-expt key-pub (find-loop door-pub) 20201227))
(define priv2 (modular-expt door-pub (find-loop key-pub) 20201227))

(display priv1)(newline)
(display priv2)
