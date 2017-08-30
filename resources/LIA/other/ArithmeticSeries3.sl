(set-logic LIA)

(synth-fun arithmeticseries3 ((a Int) (b Int) (c Int)) Int

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
      
(constraint (=> (= (- b a) (- c b)) (= (arithmeticseries3 a b c) 1)))
(constraint (=> (not (= (- b a) (- c b))) (= (arithmeticseries3 a b c) 0)))
(check-synth)
