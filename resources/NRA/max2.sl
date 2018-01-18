(set-logic QF_NRA)
(synth-fun max ((a Real)(b Real)) Real)

(constraint (= (max 0.0 0.0) 0.0))
(constraint (= (max 2.0 0.0) 2.0))
(constraint (= (max 0.0 300.0) 300.0))
(constraint (= (max 2.0 240.0) 240.0))
(constraint (= (max (- 7.0) (- 9.0)) (- 7.0)))

(check-synth)
