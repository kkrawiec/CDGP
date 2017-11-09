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

(constraint (>= a 0))
(constraint (>= b 0))
(constraint (>= c 0))
(constraint (>= d 0))
(constraint (= (+ a b c d) ElementsSum))



; '(not (a * (a + b + c + d) - (a + b) * (a + c) > 0) or res > 0) and ' \
; '(not (a * (a + b + c + d) - (a + b) * (a + c) > 0) or res < 0) and ' \
; '(not (a * (a + b + c + d) - (a + b) * (a + c) > 0) or res == 0)'

(define-fun condition ((a Int)(b Int)(c Int)(d Int)) Int 
    (- (* a ElementsSum) (* (+ a b) (+ a c)))
)

(constraint (=> (> (condition a b c d) 0)
                (> (rsconf a b c d) 0)))

(constraint (=> (< (condition a b c d) 0)
                (< (rsconf a b c d) 0)))

(constraint (=> (= (condition a b c d) 0)
                (= (rsconf a b c d) 0)))

(check-synth)

