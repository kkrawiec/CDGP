(set-logic LIA)

(synth-fun countpositive2 ((a Int) (b Int)) Int

((Start Int (a b 0 1
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

(constraint (=> (and (<= a 0) (<= b 0)) (= (countpositive2 a b) 0)))
(constraint (=> (and (> a 0)  (<= b 0)) (= (countpositive2 a b) 1)))
(constraint (=> (and (<= a 0) (> b 0))  (= (countpositive2 a b) 1)))
(constraint (=> (and (> a 0)  (> b 0))  (= (countpositive2 a b) 2)))      
(check-synth)
