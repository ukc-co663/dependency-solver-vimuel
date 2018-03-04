(set-option :produce-models true)

;; SORTS
(declare-sort Pkg 0)
(define-sort Time () Int)

;; CONSTANTS
(declare-const t-final Time)

;; FUNCTIONS
(declare-fun installed (Pkg Time) Bool)

;;;; At least one of ps is installed at t
(define-fun-rec at-least-one-installed-of ((ps (List Pkg)) (t Time)) Bool
  (ite (= nil ps) false (or (and (installed (head ps) t) (installed (head ps) (- t 1))) (at-least-one-installed-of (tail ps) t))))

;;;; Package p depends on one of the packages in ps
(define-fun depends ((p Pkg) (ps (List Pkg))) Bool
(forall ((t Time)) (=> (installed p t)
                       (and (at-least-one-installed-of ps t)
                            (=> (> t 0) (at-least-one-installed-of ps (- t 1)))))))

; (define-fun conflicts ((p1 Pkg) (p2 Pkg)) Bool
; (forall ((t Time)) (not (and (installed p1 t) (or (installed p2 t))))))

(define-fun conflicts ((p1 Pkg) (p2 Pkg)) Bool
(forall ((t Time)) (and (=> (installed p1 t)
                          (not (or (installed p2 (+ t 1)) (installed p2 t) (installed p2 (- t 1)))))
                        (=> (installed p2 t)
                          (not (or (installed p1 (+ t 1)) (installed p1 t) (installed p1 (- t 1))))))))

;;;; Last state is always at time t > 0
(assert (> t-final 0))

;;;; Nothing is installed before t = 0
(assert (forall ((t Time) (p Pkg)) (=> (< t 0) (not (installed p t)))))

;; INVARIANTS
;;;; Only one package may be (un)installed at a time
(assert (forall ((t Time) (p Pkg) (q Pkg))
                (=> (and (not (= (installed p t) (installed p (+ t 1))))
                         (not (= (installed q t) (installed q (+ t 1)))))
                    (= p q))))
(minimize t-final)
