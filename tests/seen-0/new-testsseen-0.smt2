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
(declare-const |A=2| Pkg) ; 6
(assert (not (installed |A=2| 0))) ; 7
(declare-const |A=3| Pkg) ; 8
(assert (not (installed |A=3| 0))) ; 9
(declare-const |A=0| Pkg) ; 10
(assert (not (installed |A=0| 0))) ; 11
(declare-const |A=8| Pkg) ; 12
(assert (not (installed |A=8| 0))) ; 13
(declare-const |A=4| Pkg) ; 14
(assert (not (installed |A=4| 0))) ; 15
(declare-const |A=7| Pkg) ; 16
(assert (not (installed |A=7| 0))) ; 17
(declare-const |A=5| Pkg) ; 18
(assert (not (installed |A=5| 0))) ; 19
(declare-const |A=9| Pkg) ; 20
(assert (not (installed |A=9| 0))) ; 21
(declare-const |A=6| Pkg) ; 22
(assert (not (installed |A=6| 0))) ; 23
(assert (depends |_VIRTUAL_=| (insert |A=6|(insert |A=9|(insert |A=5|(insert |A=7|(insert |A=4|(insert |A=8|(insert |A=0|(insert |A=3|(insert |A=2|(insert |A=1| nil)))))))))))) ; 24
(assert (installed |_VIRTUAL_=| t-final)) ; 25
(check-sat) ; 26
(eval t-final) ; 27
(get-model) ; 28
(exit) ; 29
(declare-sort Pkg) ; A package
(define-sort Time () Int) ; The installation step

(declare-fun installed (Pkg Time) Bool) ; Pkg is installed at time t

; For all of the dependencies, at least one has to be installed
(define-fun-rec at-least-one-installed-of ((ps (List Pkg)) (t Time)) Bool
  (ite (= nil ps) false (or (and (installed (head ps) t) (=> (> t 1) (installed (head ps) (- t 1)))) (at-least-one-installed-of (tail ps) t))))

(define-fun depends ((p Pkg) (ps (List Pkg))) Bool
  (forall ((t Time)) (=> (installed p t) (at-least-one-installed-of ps t))))

(define-fun confl ((p1 Pkg) (p2 Pkg)) Bool
  (forall ((t Time)) (and (=> (and (installed p1 t) (installed p2 (- t 2)))
                              (not (or (installed p2 (- t 1)) (installed p2 t))))
                          (=> (and (installed p2 t) (installed p1 (- t 2)))
                              (not (or (installed p1 (- t 1)) (installed p1 t)))))))

(assert (forall ((t Time) (p Pkg)) (=> (< t 0) (not (installed p t))))) ; unsat without this

(declare-const t-final Time)
(assert (> t-final 0))
; (minimize t-final)
(assert (< t-final 10))
 ; 1
(declare-const |_VIRTUAL_=| Pkg) ; 2
(assert (not (installed |_VIRTUAL_=| 0))) ; 3
(declare-const |A=1| Pkg) ; 4
(assert (not (installed |A=1| 0))) ; 5
(declare-const |A=2| Pkg) ; 6
(assert (not (installed |A=2| 0))) ; 7
(declare-const |A=3| Pkg) ; 8
(assert (not (installed |A=3| 0))) ; 9
(declare-const |A=0| Pkg) ; 10
(assert (not (installed |A=0| 0))) ; 11
(declare-const |A=8| Pkg) ; 12
(assert (not (installed |A=8| 0))) ; 13
(declare-const |A=4| Pkg) ; 14
(assert (not (installed |A=4| 0))) ; 15
(declare-const |A=7| Pkg) ; 16
(assert (not (installed |A=7| 0))) ; 17
(declare-const |A=5| Pkg) ; 18
(assert (not (installed |A=5| 0))) ; 19
(declare-const |A=9| Pkg) ; 20
(assert (not (installed |A=9| 0))) ; 21
(declare-const |A=6| Pkg) ; 22
(assert (not (installed |A=6| 0))) ; 23
(assert (depends |_VIRTUAL_=| (insert |A=6|(insert |A=9|(insert |A=5|(insert |A=7|(insert |A=4|(insert |A=8|(insert |A=0|(insert |A=3|(insert |A=2|(insert |A=1| nil)))))))))))) ; 24
(assert (installed |_VIRTUAL_=| t-final)) ; 25
(check-sat)
 ; 26
(eval t-final)
 ; 27
