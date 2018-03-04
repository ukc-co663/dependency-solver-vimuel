(declare-sort Pkg)
(define-sort Time () Int)

(declare-fun installed (Pkg Time) Bool) ; Pkg is installed at time t

(declare-fun confl (Pkg Pkg) Bool)

; for each member m of set, call installed m t
(define-fun-rec foo ((ps (List Pkg)) (t Time)) Bool
  (ite (= nil ps) false (or (installed (head ps) t) (foo (tail ps) t))))


(define-fun depends ((p Pkg) (ps (List Pkg))) Bool
  (forall ((t Time)) (=> (installed p t) (and (foo ps t) (foo ps (- t 1))))))


; (define-fun depends (Pkg (List Pkg)) Bool
;   (forall ((t Time)) (=> (installed p (+ t 1))
;                          (and (foo ps t) (foo ps (+ t 1))))))

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
; (assert (forall
;   ((t Time))
;   (=> (installed d (+ t 1)) (foo (insert b (insert c nil)) t) (foo (insert b (insert c nil)) (+ t 1)))))

(assert (depends d (insert b (insert c nil))))
(assert (depends c (insert b nil)))
(assert (depends b (insert c (insert a nil))))
(assert (confl d a))
(assert (forall ((t Time)) (=> (< t 0) (not (installed a t)))))

(assert (installed a 0))
(assert (not (installed b 0)))
(assert (not (installed c 0)))
(assert (not (installed d 0)))
(assert (installed d 3))
; (assert (installed c 2))



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
