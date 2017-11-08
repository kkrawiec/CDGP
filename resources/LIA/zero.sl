(set-logic LIA)
(synth-fun f ( (x Int)) Int )
(declare-var x1 Int)
(constraint (= (f x1) (* 2 (f x1))))
(check-synth)

