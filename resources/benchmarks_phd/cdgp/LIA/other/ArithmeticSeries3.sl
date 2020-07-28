(set-logic LIA)

(synth-fun arithmeticseries3 ((a Int) (b Int) (c Int)) Int)

(declare-var a Int)
(declare-var b Int)
(declare-var c Int)
      
(constraint (=> (= (- b a) (- c b)) (= (arithmeticseries3 a b c) 1)))
(constraint (=> (not (= (- b a) (- c b))) (= (arithmeticseries3 a b c) 0)))
(check-synth)
