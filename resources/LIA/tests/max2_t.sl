(set-logic LIA)
(synth-fun max ((a Int)(b Int)) Int)

(constraint (= (max 0 0) 0))
(constraint (= (max 2 0) 2))
(constraint (= (max 0 300) 300))
(constraint (= (max 2 240) 240))
(constraint (= (max (- 7) (- 9)) (- 7)))

(check-synth)
