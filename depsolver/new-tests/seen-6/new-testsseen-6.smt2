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
(declare-const |X4=1| Pkg) ; 10
(assert (not (installed |X4=1| 0))) ; 11
(declare-const |X5=1| Pkg) ; 12
(assert (not (installed |X5=1| 0))) ; 13
(declare-const |X1=0| Pkg) ; 14
(assert (not (installed |X1=0| 0))) ; 15
(declare-const |X2=0| Pkg) ; 16
(assert (not (installed |X2=0| 0))) ; 17
(declare-const |X3=0| Pkg) ; 18
(assert (not (installed |X3=0| 0))) ; 19
(declare-const |X4=0| Pkg) ; 20
(assert (not (installed |X4=0| 0))) ; 21
(declare-const |X5=0| Pkg) ; 22
(assert (not (installed |X5=0| 0))) ; 23
(assert (depends |_VIRTUAL_=| (insert |X1=1| nil))) ; 24
(assert (depends |_VIRTUAL_=| (insert |X2=0| nil))) ; 25
(assert (depends |_VIRTUAL_=| (insert |X3=0| nil))) ; 26
(assert (depends |_VIRTUAL_=| (insert |X4=1| nil))) ; 27
(assert (depends |_VIRTUAL_=| (insert |X5=0| nil))) ; 28
(assert (depends |X1=1| (insert |X2=1|(insert |X3=1|(insert |X4=1| nil))))) ; 29
(assert (depends |X1=1| (insert |X2=1|(insert |X4=0|(insert |X5=0| nil))))) ; 30
(assert (depends |X1=1| (insert |X3=0|(insert |X4=0|(insert |X5=0| nil))))) ; 31
(assert (depends |X1=1| (insert |X2=0|(insert |X4=0|(insert |X5=0| nil))))) ; 32
(assert (depends |X4=1| (insert |X1=1|(insert |X3=1|(insert |X5=1| nil))))) ; 33
(assert (installed |_VIRTUAL_=| t-final)) ; 34
(check-sat) ; 35
(eval t-final) ; 36
(get-model) ; 37
(exit) ; 38
