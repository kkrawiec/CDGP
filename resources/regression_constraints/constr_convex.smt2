(echo "convex: x^2 (solved only for t=0.5)")
(set-logic NRA)
(set-option :pp.decimal true)
(set-option :produce-models true)

(define-fun f ((x Real)) Real (* x x) )  ; x^2 is an example of a convex function

(declare-fun x () Real)
(declare-fun cdgp.x () Real)

(assert (not (=> (> cdgp.x x)
   (<= (f (/ (+ x cdgp.x) 2.0))
       (/ (+ (f x) (f cdgp.x)) 2.0)))))


(check-sat)
(get-model)
(reset)
(echo "")
(echo "----------------------------")
(echo "convex: x^2 (solved with quantifier)")


(define-fun f ((x Real)) Real (* x x) )  ; x^2 is an example of a convex function

(declare-fun x () Real)
(declare-fun cdgp.x () Real)

(assert (not (forall ((t Int)) (=> (and (> cdgp.x x) (>= t 0.0) (<= t 1.0))
    (<= (f (+ (* t x) (* (- 1.0 t) cdgp.x)))
        (+ (* t (f x)) (* (- 1.0 t) (f cdgp.x))))
))))


(check-sat)
(get-model)
(reset)
(echo "")
(echo "----------------------------")
(echo "convex: x^2 (solved for any t without quantifier)")



; This version with declaring t and searching for its value so that implication does not hold works best

(define-fun f ((x Real)) Real (* x x) )  ; x^2 is an example of a convex function

(declare-fun x () Real)
(declare-fun cdgp.x () Real)
(declare-fun cdgp.t () Real)

(assert (not (=> (> cdgp.x x)
    (=> (and (>= cdgp.t 0.0) (<= cdgp.t 1.0))
        (<= (f (+ (* cdgp.t x) (* (- 1.0 cdgp.t) cdgp.x)))
            (+ (* cdgp.t (f x)) (* (- 1.0 cdgp.t) (f cdgp.x))))
))))


(check-sat)
(get-model)
(reset)
(echo "")
(echo "----------------------------")
(echo "non-convex: x^3")


(define-fun f ((x Real)) Real (* x x x) )  ; x^3 is not convex

(declare-fun x () Real)
(declare-fun cdgp.x () Real)
(declare-fun cdgp.t () Real)

(assert (not (=> (> cdgp.x x)
    (=> (and (>= cdgp.t 0.0) (<= cdgp.t 1.0))
        (<= (f (+ (* cdgp.t x) (* (- 1.0 cdgp.t) cdgp.x)))
            (+ (* cdgp.t (f x)) (* (- 1.0 cdgp.t) (f cdgp.x))))
))))


(check-sat)
(get-model)
(reset)
(echo "")
(echo "----------------------------")
(echo "convex: x")



(define-fun f ((x Real)) Real x )  ; f(x)=x is an example of convex but not strictly convex function

(declare-fun x () Real)
(declare-fun cdgp.x () Real)
(declare-fun cdgp.t () Real)

(assert (not (=> (> cdgp.x x)
    (=> (and (>= cdgp.t 0.0) (<= cdgp.t 1.0))
        (<= (f (+ (* cdgp.t x) (* (- 1.0 cdgp.t) cdgp.x)))
            (+ (* cdgp.t (f x)) (* (- 1.0 cdgp.t) (f cdgp.x))))
))))

(check-sat)
(get-model)
(reset)
(echo "")
(echo "----------------------------")
(echo "not strictly convex: x")



(define-fun f ((x Real)) Real x )  ; f(x)=x is an example of convex but not strictly convex function

(declare-fun x () Real)
(declare-fun cdgp.x () Real)
(declare-fun cdgp.t () Real)

(assert (not (=> (> cdgp.x x)
    (=> (and (>= cdgp.t 0.0) (<= cdgp.t 1.0))
        (< (f (+ (* cdgp.t x) (* (- 1.0 cdgp.t) cdgp.x)))
            (+ (* cdgp.t (f x)) (* (- 1.0 cdgp.t) (f cdgp.x))))
))))

(check-sat)
(get-model)
