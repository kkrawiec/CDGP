(set-logic LIA)

(synth-fun median3 ((a Int) (b Int) (c Int)) Int

((Start Int (a b c 0 1
    (+ Start Start)
    (- Start Start)
    (ite StartBool Start Start)))

    (StartBool Bool ((and StartBool StartBool)
        (or StartBool StartBool)
        (=> StartBool StartBool)
        (not StartBool)
        (<= Start Start)
        (= Start Start)
        (>= Start Start)))))

(declare-var a Int)
(declare-var b Int)
(declare-var c Int)

; Test cases
(constraint (= (median3 0 1 2) 1) )
(constraint (= (median3 0 0 0) 0) )
(constraint (= (median3 -12 -19 -14) -14) )
(constraint (= (median3 -90 0 180) 0) )
(constraint (= (median3 20 30 40) 30) )
(constraint (= (median3 20 40 30) 30) )
(constraint (= (median3 30 20 40) 30) )
(constraint (= (median3 30 40 20) 30) )
(constraint (= (median3 40 20 30) 30) )
(constraint (= (median3 40 30 20) 30) )

; Formal spec
;(constraint (=> (> a b) (=> (> b c) (= (median3 a b c) b) ) ))
;(constraint (=> (> a b) (=> (<= b c) (=> (> a c) (= (median3 a b c) c) ) ) ) )
;(constraint (=> (> a b) (=> (<= b c) (=> (<= a c ) (= (median3 a b c) a) ) ) ) )
;(constraint (=> (<= a b) (=> (> a c) (= (median3 a b c) a) ) ) )
;(constraint (=> (<= a b) (=> (<= a c) (=> (> b c) (= (median3 a b c) c) ) ) ) )
;(constraint (=> (<= a b) (=> (<= a c) (=> (<= b c ) (= (median3 a b c) b) ) ) ) )
      
(check-synth)


