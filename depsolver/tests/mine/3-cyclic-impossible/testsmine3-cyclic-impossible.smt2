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

(declare-const t-final Time)
; (assert (> t-final 0))"
; (minimize t-final)"
(assert (= t-final 5))
 ; 1
(declare-const |_VIRTUAL_=| Pkg) ; 2
(assert (not (installed |_VIRTUAL_=| 0))) ; 3
(declare-const |A=2.1| Pkg) ; 4
(assert (not (installed |A=2.1| 0))) ; 5
(declare-const |B=3.0| Pkg) ; 6
(assert (not (installed |B=3.0| 0))) ; 7
(declare-const |B=3.2| Pkg) ; 8
(assert (not (installed |B=3.2| 0))) ; 9
(declare-const |C=1| Pkg) ; 10
(assert (not (installed |C=1| 0))) ; 11
(declare-const |D=10.3.1| Pkg) ; 12
(assert (not (installed |D=10.3.1| 0))) ; 13
(declare-const |E=2.1| Pkg) ; 14
(assert (not (installed |E=2.1| 0))) ; 15
(declare-const |F=3.0| Pkg) ; 16
(assert (not (installed |F=3.0| 0))) ; 17
(declare-const |F=3.2| Pkg) ; 18
(assert (not (installed |F=3.2| 0))) ; 19
(declare-const |G=1| Pkg) ; 20
(assert (not (installed |G=1| 0))) ; 21
(declare-const |H=10.3.1| Pkg) ; 22
(assert (not (installed |H=10.3.1| 0))) ; 23
(assert (depends |_VIRTUAL_=| (insert |A=2.1| nil))) ; 24
(assert (depends |A=2.1| (insert |B=3.2|(insert |C=1| nil)))) ; 25
(assert (depends |A=2.1| (insert |D=10.3.1| nil))) ; 26
(assert (depends |D=10.3.1| (insert |E=2.1| nil))) ; 27
(assert (depends |E=2.1| (insert |F=3.2|(insert |G=1| nil)))) ; 28
(assert (depends |E=2.1| (insert |H=10.3.1| nil))) ; 29
(assert (depends |E=2.1| (insert |D=10.3.1| nil))) ; 30
(assert (installed |_VIRTUAL_=| t-final)) ; 31
(check-sat) ; 32
(eval t-final) ; 33
(get-model) ; 34
(exit) ; 35
