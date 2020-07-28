(set-logic LIA)

(synth-fun countpositive3 ((a Int) (b Int) (c Int)) Int)

(declare-var a Int)
(declare-var b Int)
(declare-var c Int)

(constraint (=> (and (<= a 0) (and (<= b 0) (<= c 0))) (= (countpositive3 a b c) 0)))
(constraint (=> (and (<= a 0) (and (<= b 0) (> c 0)))  (= (countpositive3 a b c) 1))) 
(constraint (=> (and (<= a 0) (and (> b 0)  (<= c 0))) (= (countpositive3 a b c) 1)))
(constraint (=> (and (> a 0)  (and (<= b 0) (<= c 0))) (= (countpositive3 a b c) 1)))
(constraint (=> (and (> a 0) (and (> b 0) (<= c 0))) (= (countpositive3 a b c) 2)))
(constraint (=> (and (> a 0) (and (<= b 0) (> c 0))) (= (countpositive3 a b c) 2)))
(constraint (=> (and (<= a 0) (and (> b 0) (> c 0))) (= (countpositive3 a b c) 2)))
(constraint (=> (and (> a 0) (and (> b 0) (> c 0))) (= (countpositive3 a b c) 3)))      
(check-synth)
