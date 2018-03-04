(declare-sort Pkg)
(define-sort Time () Int)

(declare-fun order ((Pkg)) Time)

(define-fun installed ((p Pkg)) Bool (>= (order p) 0))

(declare-fun installed-at-t (Pkg Time) Bool)

(define-fun depends ((p1 Pkg) (p2 Pkg)) Bool
  (=> (installed p1) (> (order p1) (order p2))))

(define-fun conflicts ((p1 Pkg) (p2 Pkg)) Bool
