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

(constraint (=> (> a b) (=> (> b c) (= (median3 a b c) b) ) ))
(constraint (=> (> a b) (=> (<= b c) (=> (> a c) (= (median3 a b c) c) ) ) ) )
(constraint (=> (> a b) (=> (<= b c) (=> (<= a c ) (= (median3 a b c) a) ) ) ) )
(constraint (=> (<= a b) (=> (> a c) (= (median3 a b c) a) ) ) )
(constraint (=> (<= a b) (=> (<= a c) (=> (> b c) (= (median3 a b c) c) ) ) ) )
(constraint (=> (<= a b) (=> (<= a c) (=> (<= b c ) (= (median3 a b c) b) ) ) ) )
      
(check-synth)


