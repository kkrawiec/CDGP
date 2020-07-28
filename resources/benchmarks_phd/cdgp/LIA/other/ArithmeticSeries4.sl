(set-logic LIA)

(synth-fun arithmeticseries4 ((a Int) (b Int) (c Int) (d Int)) Int)

(declare-var a Int)
(declare-var b Int)
(declare-var c Int)
(declare-var d Int)
      
(constraint (=> (and (= (- b a) (- c b)) (= (- c b) (- d c))) (= (arithmeticseries4 a b c d) 1)))
(constraint (=> (not (and (= (- b a) (- c b)) (= (- c b) (- d c)))) (= (arithmeticseries4 a b c d) 0)))
(check-synth)
