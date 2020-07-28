(set-logic LIA)

(synth-fun countpositive2 ((a Int) (b Int)) Int)

(declare-var a Int)
(declare-var b Int)

(constraint (=> (and (<= a 0) (<= b 0)) (= (countpositive2 a b) 0)))
(constraint (=> (and (> a 0)  (<= b 0)) (= (countpositive2 a b) 1)))
(constraint (=> (and (<= a 0) (> b 0))  (= (countpositive2 a b) 1)))
(constraint (=> (and (> a 0)  (> b 0))  (= (countpositive2 a b) 2)))      
(check-synth)
