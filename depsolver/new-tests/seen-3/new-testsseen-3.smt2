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
(assert (> t-final 0))
(minimize t-final)
(assert (< t-final 100))
 ; 1
(declare-const |_VIRTUAL_=| Pkg) ; 2
(assert (not (installed |_VIRTUAL_=| 0))) ; 3
(declare-const |A=1| Pkg) ; 4
(assert (not (installed |A=1| 0))) ; 5
(declare-const |B=1| Pkg) ; 6
(assert (not (installed |B=1| 0))) ; 7
(declare-const |C=1| Pkg) ; 8
(assert (not (installed |C=1| 0))) ; 9
(declare-const |D=1| Pkg) ; 10
(assert (not (installed |D=1| 0))) ; 11
(assert (depends |_VIRTUAL_=| (insert |A=1| nil))) ; 12
(assert (depends |A=1| (insert |B=1|(insert |C=1| nil)))) ; 13
(assert (depends |B=1| (insert |D=1| nil))) ; 14
(assert (depends |D=1| (insert |A=1| nil))) ; 15
(assert (installed |_VIRTUAL_=| t-final)) ; 16
(check-sat) ; 17
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
(assert (> t-final 0))
(minimize t-final)
(assert (< t-final 100))
 ; 1
(declare-const |_VIRTUAL_=| Pkg) ; 2
(assert (not (installed |_VIRTUAL_=| 0))) ; 3
(declare-const |A=1| Pkg) ; 4
(assert (not (installed |A=1| 0))) ; 5
(declare-const |B=1| Pkg) ; 6
(assert (not (installed |B=1| 0))) ; 7
(declare-const |C=1| Pkg) ; 8
(assert (not (installed |C=1| 0))) ; 9
(declare-const |D=1| Pkg) ; 10
(assert (not (installed |D=1| 0))) ; 11
(assert (depends |_VIRTUAL_=| (insert |A=1| nil))) ; 12
(assert (depends |A=1| (insert |B=1|(insert |C=1| nil)))) ; 13
(assert (depends |B=1| (insert |D=1| nil))) ; 14
(assert (depends |D=1| (insert |A=1| nil))) ; 15
(assert (installed |_VIRTUAL_=| t-final)) ; 16
(check-sat) ; 17
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
(assert (> t-final 0))
(minimize t-final)
(assert (< t-final 100))
 ; 1
(declare-const |_VIRTUAL_=| Pkg) ; 2
(assert (not (installed |_VIRTUAL_=| 0))) ; 3
(declare-const |A=1| Pkg) ; 4
(assert (not (installed |A=1| 0))) ; 5
(declare-const |B=1| Pkg) ; 6
(assert (not (installed |B=1| 0))) ; 7
(declare-const |C=1| Pkg) ; 8
(assert (not (installed |C=1| 0))) ; 9
(declare-const |D=1| Pkg) ; 10
(assert (not (installed |D=1| 0))) ; 11
(assert (depends |_VIRTUAL_=| (insert |A=1| nil))) ; 12
(assert (depends |A=1| (insert |B=1|(insert |C=1| nil)))) ; 13
(assert (depends |B=1| (insert |D=1| nil))) ; 14
(assert (depends |D=1| (insert |A=1| nil))) ; 15
(assert (installed |_VIRTUAL_=| t-final)) ; 16
(check-sat)
 ; 17
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
(assert (> t-final 0))
(minimize t-final)
(assert (< t-final 100))
 ; 1
(declare-const |_VIRTUAL_=| Pkg) ; 2
(assert (not (installed |_VIRTUAL_=| 0))) ; 3
(declare-const |A=1| Pkg) ; 4
(assert (not (installed |A=1| 0))) ; 5
(declare-const |B=1| Pkg) ; 6
(assert (not (installed |B=1| 0))) ; 7
(declare-const |C=1| Pkg) ; 8
(assert (not (installed |C=1| 0))) ; 9
(declare-const |D=1| Pkg) ; 10
(assert (not (installed |D=1| 0))) ; 11
(assert (depends |_VIRTUAL_=| (insert |A=1| nil))) ; 12
(assert (depends |A=1| (insert |B=1|(insert |C=1| nil)))) ; 13
(assert (depends |B=1| (insert |D=1| nil))) ; 14
(assert (depends |D=1| (insert |A=1| nil))) ; 15
(assert (installed |_VIRTUAL_=| t-final)) ; 16
(check-sat)
 ; 17
(check-sat)
 ; 18
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
(assert (> t-final 0))
(minimize t-final)
(assert (< t-final 100))
 ; 1
(declare-const |_VIRTUAL_=| Pkg) ; 2
(assert (not (installed |_VIRTUAL_=| 0))) ; 3
(declare-const |A=1| Pkg) ; 4
(assert (not (installed |A=1| 0))) ; 5
(declare-const |B=1| Pkg) ; 6
(assert (not (installed |B=1| 0))) ; 7
(declare-const |C=1| Pkg) ; 8
(assert (not (installed |C=1| 0))) ; 9
(declare-const |D=1| Pkg) ; 10
(assert (not (installed |D=1| 0))) ; 11
(assert (depends |_VIRTUAL_=| (insert |A=1| nil))) ; 12
(assert (depends |A=1| (insert |B=1|(insert |C=1| nil)))) ; 13
(assert (depends |B=1| (insert |D=1| nil))) ; 14
(assert (depends |D=1| (insert |A=1| nil))) ; 15
(assert (installed |_VIRTUAL_=| t-final)) ; 16
(check-sat) ; 17
(eval t-final) ; 18
(get-model)
 ; 19
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
(assert (> t-final 0))
(minimize t-final)
(assert (< t-final 100))
 ; 1
(declare-const |_VIRTUAL_=| Pkg) ; 2
(assert (not (installed |_VIRTUAL_=| 0))) ; 3
(declare-const |A=1| Pkg) ; 4
(assert (not (installed |A=1| 0))) ; 5
(declare-const |B=1| Pkg) ; 6
(assert (not (installed |B=1| 0))) ; 7
(declare-const |C=1| Pkg) ; 8
(assert (not (installed |C=1| 0))) ; 9
(declare-const |D=1| Pkg) ; 10
(assert (not (installed |D=1| 0))) ; 11
(assert (depends |_VIRTUAL_=| (insert |A=1| nil))) ; 12
(assert (depends |A=1| (insert |B=1|(insert |C=1| nil)))) ; 13
(assert (depends |B=1| (insert |D=1| nil))) ; 14
(assert (depends |D=1| (insert |A=1| nil))) ; 15
(assert (installed |_VIRTUAL_=| t-final)) ; 16
(check-sat) ; 17
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
(assert (> t-final 0))
(minimize t-final)
(assert (< t-final 100))
 ; 1
(declare-const |_VIRTUAL_=| Pkg) ; 2
(assert (not (installed |_VIRTUAL_=| 0))) ; 3
(declare-const |A=1| Pkg) ; 4
(assert (not (installed |A=1| 0))) ; 5
(declare-const |B=1| Pkg) ; 6
(assert (not (installed |B=1| 0))) ; 7
(declare-const |C=1| Pkg) ; 8
(assert (not (installed |C=1| 0))) ; 9
(declare-const |D=1| Pkg) ; 10
(assert (not (installed |D=1| 0))) ; 11
(assert (depends |_VIRTUAL_=| (insert |A=1| nil))) ; 12
(assert (depends |A=1| (insert |B=1|(insert |C=1| nil)))) ; 13
(assert (depends |B=1| (insert |D=1| nil))) ; 14
(assert (depends |D=1| (insert |A=1| nil))) ; 15
(assert (installed |_VIRTUAL_=| t-final)) ; 16
(check-sat)
 ; 17
