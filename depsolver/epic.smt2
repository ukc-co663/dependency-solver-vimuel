(declare-sort Pkg) ; A package
(define-sort Time () Int) ; The installation step

(declare-fun installed (Pkg Time) Bool) ; Pkg is installed at time t

; For all of the dependencies, at least one has to be installed
(define-fun-rec at-least-one-installed-of ((ps (List Pkg)) (t Time)) Bool
  (ite (= nil ps) false (or (and (installed (head ps) t) (installed (head ps) (- t 1))) (at-least-one-installed-of (tail ps) t))))

(define-fun depends ((p Pkg) (ps (List Pkg))) Bool
  (forall ((t Time)) (=> (installed p t) (at-least-one-installed-of ps t))))

(define-fun confl ((p1 Pkg) (p2 Pkg)) Bool
  (forall ((t Time)) (and (=> (and (installed p1 t) (installed p2 (- t 2)))
                              (not (or (installed p2 (- t 1)) (installed p2 t))))
                          (=> (and (installed p2 t) (installed p1 (- t 2)))
                              (not (or (installed p1 (- t 1)) (installed p1 t)))))))

(assert (forall ((t Time) (p Pkg)) (=> (< t 0) (not (installed p t))))) ; unsat without this

; the bootstrapping problem
(declare-const a Pkg)
(declare-const b Pkg)
(declare-const c Pkg)
(declare-const d Pkg)
(declare-const t-final Time)

; dependencies
(assert (depends d (insert b (insert c nil))))
(assert (depends c (insert b nil)))
(assert (depends b (insert c (insert a nil))))

; conflicts
(assert (confl d a))

; initial state
(assert (installed a 0))
(assert (not (installed b 0)))
(assert (not (installed c 0)))
(assert (not (installed d 0)))

; target
(assert (installed d t-final))
(assert (> t-final 0))
(minimize t-final)
; (push)
; (assert (= t-final 8))
; (check-sat)
; (pop)
; (assert (= t-final 4))
; (push)
; (check-sat)
; (pop)
; (assert (= t-final 2))
; (push)
; (check-sat)
; (pop)
; (assert (= t-final 3))
; (push)
(check-sat)
(eval t-final)



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
