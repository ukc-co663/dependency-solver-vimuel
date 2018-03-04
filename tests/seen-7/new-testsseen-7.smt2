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
(declare-const |X1=1| Pkg) ; 4
(assert (not (installed |X1=1| 0))) ; 5
(declare-const |X2=1| Pkg) ; 6
(assert (not (installed |X2=1| 0))) ; 7
(declare-const |X3=1| Pkg) ; 8
(assert (not (installed |X3=1| 0))) ; 9
(declare-const |Y11=1| Pkg) ; 10
(assert (not (installed |Y11=1| 0))) ; 11
(declare-const |Y12=1| Pkg) ; 12
(assert (not (installed |Y12=1| 0))) ; 13
(declare-const |Y13=1| Pkg) ; 14
(assert (not (installed |Y13=1| 0))) ; 15
(declare-const |Y22=1| Pkg) ; 16
(assert (not (installed |Y22=1| 0))) ; 17
(declare-const |Y23=1| Pkg) ; 18
(assert (not (installed |Y23=1| 0))) ; 19
(declare-const |Y33=1| Pkg) ; 20
(assert (not (installed |Y33=1| 0))) ; 21
(declare-const |Z11=1| Pkg) ; 22
(assert (not (installed |Z11=1| 0))) ; 23
(declare-const |Z12=1| Pkg) ; 24
(assert (not (installed |Z12=1| 0))) ; 25
(declare-const |Z13=1| Pkg) ; 26
(assert (not (installed |Z13=1| 0))) ; 27
(declare-const |Z22=1| Pkg) ; 28
(assert (not (installed |Z22=1| 0))) ; 29
(declare-const |Z23=1| Pkg) ; 30
(assert (not (installed |Z23=1| 0))) ; 31
(declare-const |Z33=1| Pkg) ; 32
(assert (not (installed |Z33=1| 0))) ; 33
(assert (depends |_VIRTUAL_=| (insert |Y23=1| nil))) ; 34
(assert (depends |Y11=1| (insert |Z11=1| nil))) ; 35
(assert (depends |Y12=1| (insert |Y11=1|(insert |Z12=1| nil)))) ; 36
(assert (depends |Y13=1| (insert |Y12=1|(insert |Z13=1| nil)))) ; 37
(assert (depends |Y22=1| (insert |Z22=1| nil))) ; 38
(assert (depends |Y23=1| (insert |Y22=1|(insert |Z23=1| nil)))) ; 39
(assert (depends |Y33=1| (insert |Z33=1| nil))) ; 40
(assert (depends |Z11=1| (insert |X1=1| nil))) ; 41
(assert (depends |Z12=1| (insert |X2=1| nil))) ; 42
(assert (depends |Z13=1| (insert |X3=1| nil))) ; 43
(assert (depends |Z22=1| (insert |Y11=1| nil))) ; 44
(assert (depends |Z22=1| (insert |X2=1| nil))) ; 45
(assert (depends |Z23=1| (insert |Y12=1| nil))) ; 46
(assert (depends |Z23=1| (insert |X3=1| nil))) ; 47
(assert (depends |Z33=1| (insert |Y22=1| nil))) ; 48
(assert (depends |Z33=1| (insert |X3=1| nil))) ; 49
(assert (installed |_VIRTUAL_=| t-final)) ; 50
(check-sat) ; 51
(eval t-final) ; 52
(get-model) ; 53
(exit) ; 54
