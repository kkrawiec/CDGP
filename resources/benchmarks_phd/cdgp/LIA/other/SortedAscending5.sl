(set-logic LIA)


(synth-fun sortedascending5 ((a Int) (b Int) (c Int) (d Int) (e Int)) Int)

(declare-var a Int)
(declare-var b Int)
(declare-var c Int)
(declare-var d Int)
(declare-var e Int)
      
(constraint (=> (and (<= a b) (<= b c) (<= c d) (<= d e)) (= (sortedascending5 a b c d e) 1)))
(constraint (=> (not (and (<= a b) (<= b c) (<= c d) (<= d e))) (= (sortedascending5 a b c d e) 0)))           
(check-synth)
