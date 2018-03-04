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
(declare-const |X15=1| Pkg) ; 4
(assert (not (installed |X15=1| 0))) ; 5
(declare-const |X4=1| Pkg) ; 6
(assert (not (installed |X4=1| 0))) ; 7
(declare-const |X5=1| Pkg) ; 8
(assert (not (installed |X5=1| 0))) ; 9
(declare-const |X9=1| Pkg) ; 10
(assert (not (installed |X9=1| 0))) ; 11
(declare-const |X17=1| Pkg) ; 12
(assert (not (installed |X17=1| 0))) ; 13
(declare-const |X16=1| Pkg) ; 14
(assert (not (installed |X16=1| 0))) ; 15
(declare-const |X1=1| Pkg) ; 16
(assert (not (installed |X1=1| 0))) ; 17
(declare-const |X12=1| Pkg) ; 18
(assert (not (installed |X12=1| 0))) ; 19
(declare-const |X18=1| Pkg) ; 20
(assert (not (installed |X18=1| 0))) ; 21
(declare-const |X19=1| Pkg) ; 22
(assert (not (installed |X19=1| 0))) ; 23
(declare-const |X14=1| Pkg) ; 24
(assert (not (installed |X14=1| 0))) ; 25
(declare-const |X0=1| Pkg) ; 26
(assert (not (installed |X0=1| 0))) ; 27
(declare-const |X11=1| Pkg) ; 28
(assert (not (installed |X11=1| 0))) ; 29
(declare-const |X2=1| Pkg) ; 30
(assert (not (installed |X2=1| 0))) ; 31
(declare-const |X10=1| Pkg) ; 32
(assert (not (installed |X10=1| 0))) ; 33
(declare-const |X7=1| Pkg) ; 34
(assert (not (installed |X7=1| 0))) ; 35
(declare-const |X13=1| Pkg) ; 36
(assert (not (installed |X13=1| 0))) ; 37
(declare-const |X3=1| Pkg) ; 38
(assert (not (installed |X3=1| 0))) ; 39
(declare-const |X6=1| Pkg) ; 40
(assert (not (installed |X6=1| 0))) ; 41
(declare-const |X8=1| Pkg) ; 42
(assert (not (installed |X8=1| 0))) ; 43
(declare-const |target=1| Pkg) ; 44
(assert (not (installed |target=1| 0))) ; 45
(assert (depends |_VIRTUAL_=| (insert |target=1| nil))) ; 46
(assert (depends |X15=1| (insert |X4=1| nil))) ; 47
(assert (depends |X15=1| (insert |X1=1| nil))) ; 48
(assert (depends |X15=1| (insert |X9=1| nil))) ; 49
(assert (depends |X15=1| (insert |X6=1| nil))) ; 50
(assert (depends |X15=1| (insert |X3=1| nil))) ; 51
(assert (depends |X15=1| (insert |X1=1| nil))) ; 52
(assert (depends |X15=1| (insert |X7=1| nil))) ; 53
(assert (depends |X15=1| (insert |X11=1| nil))) ; 54
(assert (depends |X4=1| (insert |X7=1| nil))) ; 55
(assert (depends |X4=1| (insert |X9=1| nil))) ; 56
(assert (depends |X4=1| (insert |X17=1| nil))) ; 57
(assert (depends |X4=1| (insert |X18=1| nil))) ; 58
(assert (depends |X4=1| (insert |X18=1| nil))) ; 59
(assert (depends |X4=1| (insert |X8=1| nil))) ; 60
(assert (depends |X4=1| (insert |X7=1| nil))) ; 61
(assert (depends |X4=1| (insert |X0=1| nil))) ; 62
(assert (depends |X4=1| (insert |X1=1| nil))) ; 63
(assert (depends |X4=1| (insert |X9=1| nil))) ; 64
(assert (depends |X4=1| (insert |X14=1| nil))) ; 65
(assert (depends |X5=1| (insert |X2=1| nil))) ; 66
(assert (depends |X5=1| (insert |X14=1| nil))) ; 67
(assert (depends |X5=1| (insert |X2=1| nil))) ; 68
(assert (depends |X5=1| (insert |X6=1| nil))) ; 69
(assert (depends |X5=1| (insert |X13=1| nil))) ; 70
(assert (depends |X5=1| (insert |X6=1| nil))) ; 71
(assert (depends |X5=1| (insert |X2=1| nil))) ; 72
(assert (depends |X5=1| (insert |X1=1| nil))) ; 73
(assert (depends |X5=1| (insert |X13=1| nil))) ; 74
(assert (depends |X5=1| (insert |X8=1| nil))) ; 75
(assert (depends |X5=1| (insert |X0=1| nil))) ; 76
(assert (depends |X5=1| (insert |X16=1| nil))) ; 77
(assert (depends |X5=1| (insert |X12=1| nil))) ; 78
(assert (depends |X5=1| (insert |X11=1| nil))) ; 79
(assert (depends |X5=1| (insert |X8=1| nil))) ; 80
(assert (depends |X9=1| (insert |X3=1| nil))) ; 81
(assert (depends |X9=1| (insert |X13=1| nil))) ; 82
(assert (depends |X9=1| (insert |X12=1| nil))) ; 83
(assert (depends |X9=1| (insert |X7=1| nil))) ; 84
(assert (depends |X9=1| (insert |X7=1| nil))) ; 85
(assert (depends |X9=1| (insert |X12=1| nil))) ; 86
(assert (depends |X9=1| (insert |X16=1| nil))) ; 87
(assert (depends |X9=1| (insert |X2=1| nil))) ; 88
(assert (depends |X17=1| (insert |X10=1| nil))) ; 89
(assert (depends |X17=1| (insert |X13=1| nil))) ; 90
(assert (depends |X17=1| (insert |X6=1| nil))) ; 91
(assert (depends |X17=1| (insert |X3=1| nil))) ; 92
(assert (depends |X17=1| (insert |X3=1| nil))) ; 93
(assert (depends |X17=1| (insert |X2=1| nil))) ; 94
(assert (depends |X16=1| (insert |X7=1| nil))) ; 95
(assert (depends |X16=1| (insert |X13=1| nil))) ; 96
(assert (depends |X16=1| (insert |X1=1| nil))) ; 97
(assert (depends |X16=1| (insert |X3=1| nil))) ; 98
(assert (depends |X16=1| (insert |X19=1| nil))) ; 99
(assert (depends |X16=1| (insert |X18=1| nil))) ; 100
(assert (depends |X16=1| (insert |X11=1| nil))) ; 101
(assert (depends |X16=1| (insert |X19=1| nil))) ; 102
(assert (depends |X1=1| (insert |X19=1| nil))) ; 103
(assert (depends |X1=1| (insert |X8=1| nil))) ; 104
(assert (depends |X1=1| (insert |X10=1| nil))) ; 105
(assert (depends |X1=1| (insert |X0=1| nil))) ; 106
(assert (depends |X12=1| (insert |X3=1| nil))) ; 107
(assert (depends |X12=1| (insert |X10=1| nil))) ; 108
(assert (depends |X12=1| (insert |X13=1| nil))) ; 109
(assert (depends |X12=1| (insert |X6=1| nil))) ; 110
(assert (depends |X12=1| (insert |X19=1| nil))) ; 111
(assert (depends |X12=1| (insert |X11=1| nil))) ; 112
(assert (depends |X12=1| (insert |X19=1| nil))) ; 113
(assert (depends |X18=1| (insert |X0=1| nil))) ; 114
(assert (depends |X18=1| (insert |X10=1| nil))) ; 115
(assert (depends |X18=1| (insert |X10=1| nil))) ; 116
(assert (depends |X18=1| (insert |X0=1| nil))) ; 117
(assert (depends |X18=1| (insert |X11=1| nil))) ; 118
(assert (depends |X18=1| (insert |X14=1| nil))) ; 119
(assert (depends |X19=1| (insert |X2=1| nil))) ; 120
(assert (depends |X19=1| (insert |X6=1| nil))) ; 121
(assert (depends |X19=1| (insert |X7=1| nil))) ; 122
(assert (depends |X19=1| (insert |X13=1| nil))) ; 123
(assert (depends |X19=1| (insert |X14=1| nil))) ; 124
(assert (depends |X19=1| (insert |X8=1| nil))) ; 125
(assert (depends |X14=1| (insert |X0=1| nil))) ; 126
(assert (depends |X0=1| (insert |X13=1| nil))) ; 127
(assert (depends |X0=1| (insert |X3=1| nil))) ; 128
(assert (depends |X0=1| (insert |X2=1| nil))) ; 129
(assert (depends |X11=1| (insert |X6=1| nil))) ; 130
(assert (depends |X11=1| (insert |X13=1| nil))) ; 131
(assert (depends |X11=1| (insert |X8=1| nil))) ; 132
(assert (depends |X2=1| (insert |X7=1| nil))) ; 133
(assert (depends |X2=1| (insert |X8=1| nil))) ; 134
(assert (depends |X2=1| (insert |X7=1| nil))) ; 135
(assert (depends |X2=1| (insert |X10=1| nil))) ; 136
(assert (depends |X2=1| (insert |X8=1| nil))) ; 137
(assert (depends |X10=1| (insert |X13=1| nil))) ; 138
(assert (depends |X10=1| (insert |X3=1| nil))) ; 139
(assert (depends |X10=1| (insert |X6=1| nil))) ; 140
(assert (depends |X7=1| (insert |X13=1| nil))) ; 141
(assert (depends |X7=1| (insert |X6=1| nil))) ; 142
(assert (depends |X7=1| (insert |X3=1| nil))) ; 143
(assert (depends |X13=1| (insert |X3=1| nil))) ; 144
(assert (depends |X13=1| (insert |X8=1| nil))) ; 145
(assert (depends |X13=1| (insert |X3=1| nil))) ; 146
(assert (depends |target=1| (insert |X2=1|(insert |X17=1|(insert |X7=1|(insert |X19=1| nil)))))) ; 147
(assert (installed |_VIRTUAL_=| t-final)) ; 148
(check-sat) ; 149
(eval t-final) ; 150
(get-model) ; 151
(exit) ; 152
