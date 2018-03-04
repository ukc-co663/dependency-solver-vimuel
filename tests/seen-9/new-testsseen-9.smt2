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
(declare-const |P01=0| Pkg) ; 4
(assert (installed |P01=0| 0)) ; 5
(declare-const |P02=0| Pkg) ; 6
(assert (installed |P02=0| 0)) ; 7
(declare-const |P03=0| Pkg) ; 8
(assert (installed |P03=0| 0)) ; 9
(declare-const |P04=0| Pkg) ; 10
(assert (installed |P04=0| 0)) ; 11
(declare-const |P05=0| Pkg) ; 12
(assert (installed |P05=0| 0)) ; 13
(declare-const |P06=0| Pkg) ; 14
(assert (installed |P06=0| 0)) ; 15
(declare-const |P07=0| Pkg) ; 16
(assert (installed |P07=0| 0)) ; 17
(declare-const |P08=0| Pkg) ; 18
(assert (installed |P08=0| 0)) ; 19
(declare-const |P09=0| Pkg) ; 20
(assert (installed |P09=0| 0)) ; 21
(declare-const |P10=0| Pkg) ; 22
(assert (installed |P10=0| 0)) ; 23
(declare-const |P11=0| Pkg) ; 24
(assert (installed |P11=0| 0)) ; 25
(declare-const |P12=0| Pkg) ; 26
(assert (installed |P12=0| 0)) ; 27
(declare-const |P13=0| Pkg) ; 28
(assert (installed |P13=0| 0)) ; 29
(declare-const |P14=0| Pkg) ; 30
(assert (installed |P14=0| 0)) ; 31
(declare-const |P15=0| Pkg) ; 32
(assert (installed |P15=0| 0)) ; 33
(declare-const |P16=0| Pkg) ; 34
(assert (installed |P16=0| 0)) ; 35
(declare-const |P17=0| Pkg) ; 36
(assert (installed |P17=0| 0)) ; 37
(declare-const |P18=0| Pkg) ; 38
(assert (installed |P18=0| 0)) ; 39
(declare-const |P19=0| Pkg) ; 40
(assert (installed |P19=0| 0)) ; 41
(declare-const |P20=0| Pkg) ; 42
(assert (installed |P20=0| 0)) ; 43
(declare-const |P21=0| Pkg) ; 44
(assert (installed |P21=0| 0)) ; 45
(declare-const |P22=0| Pkg) ; 46
(assert (installed |P22=0| 0)) ; 47
(declare-const |P23=0| Pkg) ; 48
(assert (installed |P23=0| 0)) ; 49
(declare-const |P24=0| Pkg) ; 50
(assert (installed |P24=0| 0)) ; 51
(declare-const |P25=0| Pkg) ; 52
(assert (installed |P25=0| 0)) ; 53
(declare-const |P26=0| Pkg) ; 54
(assert (installed |P26=0| 0)) ; 55
(declare-const |P27=0| Pkg) ; 56
(assert (installed |P27=0| 0)) ; 57
(declare-const |P28=0| Pkg) ; 58
(assert (installed |P28=0| 0)) ; 59
(declare-const |P29=0| Pkg) ; 60
(assert (installed |P29=0| 0)) ; 61
(declare-const |P30=0| Pkg) ; 62
(assert (installed |P30=0| 0)) ; 63
(declare-const |P31=0| Pkg) ; 64
(assert (installed |P31=0| 0)) ; 65
(declare-const |P32=0| Pkg) ; 66
(assert (installed |P32=0| 0)) ; 67
(declare-const |P33=0| Pkg) ; 68
(assert (installed |P33=0| 0)) ; 69
(declare-const |P34=0| Pkg) ; 70
(assert (installed |P34=0| 0)) ; 71
(declare-const |P35=0| Pkg) ; 72
(assert (installed |P35=0| 0)) ; 73
(declare-const |P36=0| Pkg) ; 74
(assert (installed |P36=0| 0)) ; 75
(declare-const |P37=0| Pkg) ; 76
(assert (installed |P37=0| 0)) ; 77
(declare-const |P38=0| Pkg) ; 78
(assert (installed |P38=0| 0)) ; 79
(declare-const |P39=0| Pkg) ; 80
(assert (installed |P39=0| 0)) ; 81
(declare-const |P40=0| Pkg) ; 82
(assert (installed |P40=0| 0)) ; 83
(declare-const |Q01=0| Pkg) ; 84
(assert (not (installed |Q01=0| 0))) ; 85
(declare-const |Q02=0| Pkg) ; 86
(assert (not (installed |Q02=0| 0))) ; 87
(declare-const |Q03=0| Pkg) ; 88
(assert (not (installed |Q03=0| 0))) ; 89
(declare-const |Q04=0| Pkg) ; 90
(assert (not (installed |Q04=0| 0))) ; 91
(declare-const |Q05=0| Pkg) ; 92
(assert (not (installed |Q05=0| 0))) ; 93
(declare-const |Q06=0| Pkg) ; 94
(assert (not (installed |Q06=0| 0))) ; 95
(declare-const |Q07=0| Pkg) ; 96
(assert (not (installed |Q07=0| 0))) ; 97
(declare-const |Q08=0| Pkg) ; 98
(assert (not (installed |Q08=0| 0))) ; 99
(declare-const |Q09=0| Pkg) ; 100
(assert (not (installed |Q09=0| 0))) ; 101
(declare-const |Q10=0| Pkg) ; 102
(assert (not (installed |Q10=0| 0))) ; 103
(declare-const |Q11=0| Pkg) ; 104
(assert (not (installed |Q11=0| 0))) ; 105
(declare-const |Q12=0| Pkg) ; 106
(assert (not (installed |Q12=0| 0))) ; 107
(declare-const |Q13=0| Pkg) ; 108
(assert (not (installed |Q13=0| 0))) ; 109
(declare-const |Q14=0| Pkg) ; 110
(assert (not (installed |Q14=0| 0))) ; 111
(declare-const |Q15=0| Pkg) ; 112
(assert (not (installed |Q15=0| 0))) ; 113
(declare-const |Q16=0| Pkg) ; 114
(assert (not (installed |Q16=0| 0))) ; 115
(declare-const |Q17=0| Pkg) ; 116
(assert (not (installed |Q17=0| 0))) ; 117
(declare-const |Q18=0| Pkg) ; 118
(assert (not (installed |Q18=0| 0))) ; 119
(declare-const |Q19=0| Pkg) ; 120
(assert (not (installed |Q19=0| 0))) ; 121
(declare-const |Q20=0| Pkg) ; 122
(assert (not (installed |Q20=0| 0))) ; 123
(declare-const |Q21=0| Pkg) ; 124
(assert (not (installed |Q21=0| 0))) ; 125
(declare-const |Q22=0| Pkg) ; 126
(assert (not (installed |Q22=0| 0))) ; 127
(declare-const |Q23=0| Pkg) ; 128
(assert (not (installed |Q23=0| 0))) ; 129
(declare-const |Q24=0| Pkg) ; 130
(assert (not (installed |Q24=0| 0))) ; 131
(declare-const |Q25=0| Pkg) ; 132
(assert (not (installed |Q25=0| 0))) ; 133
(declare-const |Q26=0| Pkg) ; 134
(assert (not (installed |Q26=0| 0))) ; 135
(declare-const |Q27=0| Pkg) ; 136
(assert (not (installed |Q27=0| 0))) ; 137
(declare-const |Q28=0| Pkg) ; 138
(assert (not (installed |Q28=0| 0))) ; 139
(declare-const |Q29=0| Pkg) ; 140
(assert (not (installed |Q29=0| 0))) ; 141
(declare-const |Q30=0| Pkg) ; 142
(assert (not (installed |Q30=0| 0))) ; 143
(declare-const |Q31=0| Pkg) ; 144
(assert (not (installed |Q31=0| 0))) ; 145
(declare-const |Q32=0| Pkg) ; 146
(assert (not (installed |Q32=0| 0))) ; 147
(declare-const |Q33=0| Pkg) ; 148
(assert (not (installed |Q33=0| 0))) ; 149
(declare-const |Q34=0| Pkg) ; 150
(assert (not (installed |Q34=0| 0))) ; 151
(declare-const |Q35=0| Pkg) ; 152
(assert (not (installed |Q35=0| 0))) ; 153
(declare-const |Q36=0| Pkg) ; 154
(assert (not (installed |Q36=0| 0))) ; 155
(declare-const |Q37=0| Pkg) ; 156
(assert (not (installed |Q37=0| 0))) ; 157
(declare-const |Q38=0| Pkg) ; 158
(assert (not (installed |Q38=0| 0))) ; 159
(declare-const |Q39=0| Pkg) ; 160
(assert (not (installed |Q39=0| 0))) ; 161
(declare-const |Q40=0| Pkg) ; 162
(assert (not (installed |Q40=0| 0))) ; 163
(assert (depends |_VIRTUAL_=| (insert |Q01=0| nil))) ; 164
(assert (depends |_VIRTUAL_=| (insert |Q02=0| nil))) ; 165
(assert (depends |_VIRTUAL_=| (insert |Q03=0| nil))) ; 166
(assert (depends |_VIRTUAL_=| (insert |Q04=0| nil))) ; 167
(assert (depends |_VIRTUAL_=| (insert |Q05=0| nil))) ; 168
(assert (depends |_VIRTUAL_=| (insert |Q06=0| nil))) ; 169
(assert (depends |_VIRTUAL_=| (insert |Q07=0| nil))) ; 170
(assert (depends |_VIRTUAL_=| (insert |Q08=0| nil))) ; 171
(assert (depends |_VIRTUAL_=| (insert |Q09=0| nil))) ; 172
(assert (depends |_VIRTUAL_=| (insert |Q10=0| nil))) ; 173
(assert (depends |_VIRTUAL_=| (insert |Q11=0| nil))) ; 174
(assert (depends |_VIRTUAL_=| (insert |Q12=0| nil))) ; 175
(assert (depends |_VIRTUAL_=| (insert |Q13=0| nil))) ; 176
(assert (depends |_VIRTUAL_=| (insert |Q14=0| nil))) ; 177
(assert (depends |_VIRTUAL_=| (insert |Q15=0| nil))) ; 178
(assert (depends |_VIRTUAL_=| (insert |Q16=0| nil))) ; 179
(assert (depends |_VIRTUAL_=| (insert |Q17=0| nil))) ; 180
(assert (depends |_VIRTUAL_=| (insert |Q18=0| nil))) ; 181
(assert (depends |_VIRTUAL_=| (insert |Q19=0| nil))) ; 182
(assert (depends |_VIRTUAL_=| (insert |Q20=0| nil))) ; 183
(assert (depends |_VIRTUAL_=| (insert |Q21=0| nil))) ; 184
(assert (depends |_VIRTUAL_=| (insert |Q22=0| nil))) ; 185
(assert (depends |_VIRTUAL_=| (insert |Q23=0| nil))) ; 186
(assert (depends |_VIRTUAL_=| (insert |Q24=0| nil))) ; 187
(assert (depends |_VIRTUAL_=| (insert |Q25=0| nil))) ; 188
(assert (depends |_VIRTUAL_=| (insert |Q26=0| nil))) ; 189
(assert (depends |_VIRTUAL_=| (insert |Q27=0| nil))) ; 190
(assert (depends |_VIRTUAL_=| (insert |Q28=0| nil))) ; 191
(assert (depends |_VIRTUAL_=| (insert |Q29=0| nil))) ; 192
(assert (depends |_VIRTUAL_=| (insert |Q30=0| nil))) ; 193
(assert (depends |_VIRTUAL_=| (insert |Q31=0| nil))) ; 194
(assert (depends |_VIRTUAL_=| (insert |Q32=0| nil))) ; 195
(assert (depends |_VIRTUAL_=| (insert |Q33=0| nil))) ; 196
(assert (depends |_VIRTUAL_=| (insert |Q34=0| nil))) ; 197
(assert (depends |_VIRTUAL_=| (insert |Q35=0| nil))) ; 198
(assert (depends |_VIRTUAL_=| (insert |Q36=0| nil))) ; 199
(assert (depends |_VIRTUAL_=| (insert |Q37=0| nil))) ; 200
(assert (depends |_VIRTUAL_=| (insert |Q38=0| nil))) ; 201
(assert (depends |_VIRTUAL_=| (insert |Q39=0| nil))) ; 202
(assert (depends |_VIRTUAL_=| (insert |Q40=0| nil))) ; 203
(assert (installed |_VIRTUAL_=| t-final)) ; 204
(check-sat) ; 205
(eval t-final) ; 206
(get-model) ; 207
(exit) ; 208
