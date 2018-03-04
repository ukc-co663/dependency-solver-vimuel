(declare-sort Pkg)
(define-sort Time () Int)

(declare-fun installed (Pkg Time) Bool) ; Pkg is installed at time t

(declare-fun depends (Pkg Pkg) Bool)

(declare-fun confl (Pkg Pkg) Bool)

(assert (forall
  ((p1 Pkg) (p2 Pkg) (t Time))
  (=> (and (depends p1 p2) (installed p1 (+ t 1))) (and (installed p2 t) (installed p2 (+ t 1))))))

(assert (forall
  ((p1 Pkg) (p2 Pkg) (t Time))
  (=> (and (or (confl p1 p2) (confl p2 p1)) (installed p1 (+ t 1))) (not (or (installed p2 t) (installed p2 (+ t 1)))))))

(declare-const a Pkg)
(declare-const b Pkg)
(declare-const c Pkg)
(declare-const d Pkg)

; ; simple thing that works
; (assert (installed b 0))
; (assert (installed a 2))
; (assert (confl a b))

; bootstrapping
(assert (installed c 2))
(assert (or (depends d b) (depends d c)))
(assert (depends c b))
(assert (or (depends b c) (depends b a)))
(assert (confl d a))
(assert (forall ((t Time)) (=> (< t 0) (not (installed a t)))))

(assert (installed a 0))
(assert (not (installed b 0)))
(assert (not (installed c 0)))
(assert (not (installed d 0)))
(assert (not (installed a 3)))
(assert (installed d 4))



(check-sat)

(echo "a")
(eval (installed a 0))
(eval (installed a 1))
(eval (installed a 2))
(eval (installed a 3))
(eval (installed a 4))

(echo "b")
(eval (installed b 0))
(eval (installed b 1))
(eval (installed b 2))
(eval (installed b 3))
(eval (installed b 4))

(echo "c")
(eval (installed c 0))
(eval (installed c 1))
(eval (installed c 2))
(eval (installed c 3))
(eval (installed c 4))

(echo "d")
(eval (installed d 0))
(eval (installed d 1))
(eval (installed d 2))
(eval (installed d 3))
(eval (installed d 4))


; (declare-const a Pkg)
; (declare-const b Pkg)
; (assert (confl a b))
; (assert (installed b 0))
; (assert (installed a 1))
; (check-sat)
; (eval (installed b 1))
