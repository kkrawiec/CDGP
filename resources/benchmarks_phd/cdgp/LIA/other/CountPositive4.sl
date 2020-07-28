(set-logic LIA)

(synth-fun countpositive4 ((a Int) (b Int) (c Int) (d Int)) Int)

(declare-var a Int)
(declare-var b Int)
(declare-var c Int)
(declare-var d Int)

(define-fun isPos ((x Int)) Int (ite (> x 0) 1 0))
(constraint (= (countpositive4 a b c d) (+ (isPos a) (isPos b) (isPos c) (isPos d)) ))      
(check-synth)
