(set-logic LIA)


(synth-fun sortedascending4 ((a Int) (b Int) (c Int) (d Int)) Int

((Start Int (a b c d 0 1
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
(declare-var d Int)
      
(constraint (=> (and (<= a b) (and (<= b c) (<= c d))) (= (sortedascending4 a b c d) 1)))
(constraint (=> (not (and (<= a b) (and (<= b c) (<= c d)))) (= (sortedascending4 a b c d) 0)))           
(check-synth)
