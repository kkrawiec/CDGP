(echo "monotonic: 1.5*x+y  (query with quantifiers; sat=OK)")
(set-logic NRA)
(set-option :pp.decimal true)
(set-option :produce-models true)

(define-fun f ((x Real)(y Real)) Real (+ (* 1.5 x) y) )
(assert (forall ((x Real)(y Real)(x1 Real)) (=> (> x1 x)  (> (f x1 y) (f x y)))))

(check-sat)
(get-model)



(reset)
(echo "")



(echo "non-monotonic: x^2+y^2  (query with quantifiers; sat=OK)")
(set-logic NRA)
(set-option :pp.decimal true)
(set-option :produce-models true)

(define-fun f ((x Real)(y Real)) Real (+ (* x x) (* y y)) )
(assert (forall ((x Real)(y Real)(x1 Real)) (=> (> x1 x)  (> (f x1 y) (f x y)))))

(check-sat)
(get-model)



(reset)
(echo "")



(echo "non-monotonic: x^2+y^2  (query without quantifiers; unsat=OK)")
(set-logic NRA)
(set-option :pp.decimal true)
(set-option :produce-models true)

(define-fun f ((x Real)(y Real)) Real (+ (* x x) (* y y)) )
(declare-fun x () Real)
(declare-fun y () Real)
(declare-fun x1 () Real)
(assert (not (=> (> x1 x)  (> (f x1 y) (f x y)))))

(check-sat)
(get-model)



(reset)
(echo "")



(echo "monotonic (not strictly): x  (query without quantifiers; unsat=OK)")
(set-logic NRA)
(set-option :pp.decimal true)
(set-option :produce-models true)

(define-fun f ((x Real)) Real x )
(declare-fun x () Real)
(declare-fun x1 () Real)
(assert (not (=> (> x1 x)  (>= (f x1) (f x)))))

(check-sat)
(get-model)



(reset)
(echo "")



(echo "monotonic (strictly): 1.5*x  (query without quantifiers; unsat=OK)")
(set-logic NRA)
(set-option :pp.decimal true)
(set-option :produce-models true)

(define-fun f ((x Real)) Real (* 1.5 x) )
(declare-fun x () Real)
(declare-fun x1 () Real)
(assert (not (=> (> x1 x)  (> (f x1) (f x)))))

(check-sat)
(get-model)
