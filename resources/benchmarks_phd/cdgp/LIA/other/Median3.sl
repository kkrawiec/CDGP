(set-logic LIA)

(synth-fun median3 ((a Int) (b Int) (c Int)) Int)

(declare-var a Int)
(declare-var b Int)
(declare-var c Int)

(constraint (=> (> a b) (=> (> b c) (= (median3 a b c) b) ) ))
(constraint (=> (> a b) (=> (<= b c) (=> (> a c) (= (median3 a b c) c) ) ) ) )
(constraint (=> (> a b) (=> (<= b c) (=> (<= a c ) (= (median3 a b c) a) ) ) ) )
(constraint (=> (<= a b) (=> (> a c) (= (median3 a b c) a) ) ) )
(constraint (=> (<= a b) (=> (<= a c) (=> (> b c) (= (median3 a b c) c) ) ) ) )
(constraint (=> (<= a b) (=> (<= a c) (=> (<= b c ) (= (median3 a b c) b) ) ) ) )
      
(check-synth)


