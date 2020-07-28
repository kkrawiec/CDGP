(set-logic LIA)

(synth-fun range3 ((a Int) (b Int) (c Int)) Int)

(declare-var a Int)
(declare-var b Int)
(declare-var c Int)

(constraint (=> (and (<= a b)  (<= b c))  (= (range3 a b c) (- c a))))
(constraint (=> (and (<= a b)  (>= b c)  (<= a c)) (= (range3 a b c) (- b a))))
(constraint (=> (and (<= a b)  (>= b c)  (>= a c)) (= (range3 a b c) (- b c))))

(constraint (=> (and (>= a b)  (<= b c)  (>= a c))  (= (range3 a b c) (- a b))))
(constraint (=> (and (>= a b)  (<= b c)  (<= a c))  (= (range3 a b c) (- c b))))
(constraint (=> (and (>= a b)  (>= b c)) (= (range3 a b c) (- a c))))
(check-synth)
