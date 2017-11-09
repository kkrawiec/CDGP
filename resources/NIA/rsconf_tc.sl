(set-logic NIA)

(synth-fun rsconf ((a Int) (b Int) (c Int) (d Int)) Int
((Start Int (a b c d
(+ Start Start)
(- Start Start)
(* Start Start))))
)

(define-fun ElementsSum () Int 32)

(declare-var a Int)
(declare-var b Int)
(declare-var c Int)
(declare-var d Int)

(define-fun precondition ((a Int)(b Int)(c Int)(d Int)) Bool
    (and (>= a 0) (>= b 0) (>= c 0) (>= d 0) (= (+ a b c d) ElementsSum)))

(define-fun condition ((a Int)(b Int)(c Int)(d Int)) Int
(- (* a ElementsSum) (* (+ a b) (+ a c)))
)


(constraint (= (rsconf 10 2 10 10) 80))

(constraint (=> (and (precondition a b c d) (> (condition a b c d) 0))
(> (rsconf a b c d) 0)))

(constraint (=> (and (precondition a b c d) (< (condition a b c d) 0))
(< (rsconf a b c d) 0)))

(constraint (=> (and (precondition a b c d) (= (condition a b c d) 0))
(= (rsconf a b c d) 0)))

(check-synth)

