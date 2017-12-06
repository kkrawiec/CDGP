(set-logic LIA)
(synth-fun max ((x Int)(y Int)) Int)
(declare-var x Int)
(declare-var y Int)

(constraint (= (max 0 0) 0))
(constraint (= (max 2 0) 2))
(constraint (= (max 0 300) 300))
(constraint (= (max 2 240) 240))
(constraint (= (max (- 7) (- 9)) (- 7)))

(check-synth)
