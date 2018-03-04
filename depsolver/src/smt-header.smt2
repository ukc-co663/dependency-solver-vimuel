(declare-sort Pkg 0)
(declare-datatypes () ((Cmd (mk-cmd (is-install-cmd Bool) (get-pkg Pkg)))))

(declare-fun depends (Pkg Pkg) Bool)
(declare-fun conflicts (Pkg Pkg) Bool)

(define-fun-rec is-installed ((p Pkg) (cs (List Cmd))) Bool
  (ite (= cs nil) ; base case
       false ; not found
       (ite (= p (get-pkg (head cs))) ; test
            (is-install-cmd (head cs)) ; result
            (is-installed p (tail cs))))) ; recursion

(define-fun all-depends-sat ((p1 Pkg) (cs (List Cmd))) Bool
  (forall ((p2 Pkg)) (=> (depends p1 p2)
                         (is-installed p2 cs))))

(define-fun no-depends-broken ((p1 Pkg) (cs (List Cmd))) Bool
  (forall ((p2 Pkg)) (=> (depends p2 p1)
                         (not (is-installed p2 cs)))))

(define-fun no-conflicts ((p1 Pkg) (cs (List Cmd))) Bool
  (forall ((p2 Pkg)) (=> (or (conflicts p1 p2) (conflicts p2 p1))
                         (not (is-installed p2 cs)))))

(define-fun-rec isValid ((cs (List Cmd))) Bool
  (ite (= cs nil)
       true ; base case
       (and (ite (is-install-cmd (head cs))
                 (and (all-depends-sat (get-pkg (head cs)) (tail cs))
                      (no-conflicts (get-pkg (head cs)) (tail cs)))
                 (no-depends-broken (get-pkg (head cs)) (tail cs)))
            (isValid (tail cs)))))

(declare-const cs-final (List Cmd))

(assert (= (head cs-final) (mk-cmd true |_VIRTUAL_=|)))
(assert (isValid cs-final))
