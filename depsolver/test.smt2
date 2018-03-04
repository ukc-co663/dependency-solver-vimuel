(set-option :fixedpoint.engine datalog)
(declare-sort pkg)
(declare-rel depends (pkg pkg))
(declare-rel confl (pkg pkg))
(declare-rel installed (pkg Int))
(declare-var a pkg)
(declare-var b pkg)
(declare-var c pkg)
(declare-var t1 Int)
(declare-var t2 Int)

; if a is installed and a depends on b or c, then b or c must have been installed before
(rule (=>
  (and (installed a t1) (depends a b) (depends a c))
  (and (or (installed b t2) (installed c t2)) (< t2 t1))))

; if a has no dependencies, then it can be installed at t=1
; do this externally? e.g. (rule (installed a 1))

; if a is in the initial set of installed packages, then (rule (installed a 0))

; if a is installed and conflicts with b, then b may not be installed at the time a is installed
(rule (=>
  (and (installed a t1) (confl a b))
  (and (not (installed b t2)) ( t2 t1)))) ; bad

(declare-const |A=1| pkg)
(declare-const |B=1| pkg)
(declare-const |C=1| pkg)

(declare-const t-final Int)

(declare-rel q1 ())
(rule (installed |A=1| t-final) q1)

(rule (depends |A=1| |B=1|))
(rule (depends |B=1| |C=1|))

(rule (installed |C=1| 0))

(query q1 :print-answer true)
