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
(declare-const |A=1| Pkg) ; 4
(assert (not (installed |A=1| 0))) ; 5
(declare-const |B=2| Pkg) ; 6
(assert (not (installed |B=2| 0))) ; 7
(declare-const |C=3| Pkg) ; 8
(assert (not (installed |C=3| 0))) ; 9
(declare-const |D=0| Pkg) ; 10
(assert (not (installed |D=0| 0))) ; 11
(declare-const |E=8| Pkg) ; 12
(assert (not (installed |E=8| 0))) ; 13
(declare-const |F=4| Pkg) ; 14
(assert (not (installed |F=4| 0))) ; 15
(declare-const |G=7| Pkg) ; 16
(assert (not (installed |G=7| 0))) ; 17
(declare-const |H=5| Pkg) ; 18
(assert (not (installed |H=5| 0))) ; 19
(declare-const |I=9| Pkg) ; 20
(assert (not (installed |I=9| 0))) ; 21
(declare-const |J=6| Pkg) ; 22
(assert (not (installed |J=6| 0))) ; 23
(assert (depends |_VIRTUAL_=| (insert |A=1| nil))) ; 24
(assert (depends |A=1| (insert |B=2| nil))) ; 25
(assert (depends |A=1| (insert |C=3| nil))) ; 26
(assert (depends |B=2| (insert |C=3| nil))) ; 27
(assert (depends |B=2| (insert |D=0| nil))) ; 28
(assert (depends |C=3| (insert |D=0| nil))) ; 29
(assert (depends |C=3| (insert |E=8| nil))) ; 30
(assert (depends |D=0| (insert |E=8|(insert |F=4| nil)))) ; 31
(assert (depends |E=8| (insert |G=7| nil))) ; 32
(assert (depends |F=4| (insert |H=5| nil))) ; 33
(assert (depends |G=7| (insert |I=9| nil))) ; 34
(assert (depends |H=5| (insert |J=6| nil))) ; 35
(assert (installed |_VIRTUAL_=| t-final)) ; 36
(check-sat) ; 37
(eval t-final) ; 38
(get-model) ; 39
(exit) ; 40
