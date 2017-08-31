(set-logic LIA)

(synth-fun range3 ((a Int) (b Int) (c Int)) Int

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

(constraint (=> (and (<= a b)  (<= b c))  (= (range3 a b c) (- c a))))
(constraint (=> (and (<= a b)  (>= b c)  (<= a c)) (= (range3 a b c) (- b a))))
(constraint (=> (and (<= a b)  (>= b c)  (>= a c)) (= (range3 a b c) (- b c))))

(constraint (=> (and (>= a b)  (<= b c)  (>= a c))  (= (range3 a b c) (- a b))))
(constraint (=> (and (>= a b)  (<= b c)  (<= a c))  (= (range3 a b c) (- c b))))
(constraint (=> (and (>= a b)  (>= b c)) (= (range3 a b c) (- a c))))
(check-synth)
