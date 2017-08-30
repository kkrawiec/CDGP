(set-logic LIA)

(synth-fun countpositive3 ((a Int) (b Int) (c Int)) Int

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

(constraint (=> (and (<= a 0) (and (<= b 0) (<= c 0))) (= (countpositive3 a b c) 0)))
(constraint (=> (and (<= a 0) (and (<= b 0) (> c 0)))  (= (countpositive3 a b c) 1))) 
(constraint (=> (and (<= a 0) (and (> b 0)  (<= c 0))) (= (countpositive3 a b c) 1)))
(constraint (=> (and (> a 0)  (and (<= b 0) (<= c 0))) (= (countpositive3 a b c) 1)))
(constraint (=> (and (> a 0) (and (> b 0) (<= c 0))) (= (countpositive3 a b c) 2)))
(constraint (=> (and (> a 0) (and (<= b 0) (> c 0))) (= (countpositive3 a b c) 2)))
(constraint (=> (and (<= a 0) (and (> b 0) (> c 0))) (= (countpositive3 a b c) 2)))
(constraint (=> (and (> a 0) (and (> b 0) (> c 0))) (= (countpositive3 a b c) 3)))      
(check-synth)
