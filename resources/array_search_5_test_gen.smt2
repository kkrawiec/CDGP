(set-logic LIA)
; (synth-fun findIdx ( (y1 Int) (y2 Int) (y3 Int) (y4 Int) (y5 Int) (k1 Int)) Int ((Start Int ( 0 1 2 3 4 5 y1 y2 y3 y4 y5 k1 (ite BoolExpr Start Start))) (BoolExpr Bool ((< Start Start) (<= Start Start) (> Start Start) (>= Start Start)))))
(declare-var x1 Int)
(declare-var x2 Int)
(declare-var x3 Int)
(declare-var x4 Int)
(declare-var x5 Int)
(declare-var k Int)


; We can constraint or set values for a given variable. In fact, this is necessary, because solver seems to work deterministically for LIA.
;(assert (= x1 0))
;(assert (= x2 1))
;(assert (= x3 2))
;(assert (= x4 3))
;(assert (= x5 4))
;(assert (= k 4))




; We create a variable for the expected result.
(declare-var result Int)
(assert (>= result -200))  ; if result=-200 in the model, then value of result could be chosen arbitrarily (i.e. function was undefined).

; In the constraints "(findIdx x1 x2 x3 x4 x5 k)" needs to be replaced with our previously declared variable ("result").
; Also, each "constraint" keyword needs to be replaced by "assert" to make this compatible with SMT-LIB 2.0.

(assert (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (< k x1) (= result 0))))
(assert (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (> k x5) (= result 5))))
(assert (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (and (> k x1) (< k x2)) (= result 1))))
(assert (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (and (> k x2) (< k x3)) (= result 2))))
(assert (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (and (> k x3) (< k x4)) (= result 3))))
(assert (=> (and (< x1 x2) (and (< x2 x3) (and (< x3 x4) (< x4 x5)))) (=> (and (> k x4) (< k x5)) (= result 4))))

; "(check-synth)" changed to "(check-sat)"
(check-sat)

; We should obtain a model which is exactly definition of a possible test case.
(get-model)
