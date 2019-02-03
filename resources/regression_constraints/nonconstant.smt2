(echo "nonconstant: x^2")
(set-logic NRA)
(set-option :pp.decimal true)
(set-option :produce-models true)

(define-fun f ((x Real)) Real (* x x) )

(declare-fun x () Real)
(declare-fun cdgp.x () Real)

(assert (not
  (distinct (f x) (f cdgp.x))
))

(check-sat)
(get-model)



(reset)
(echo "")
(echo "----------------------------")
(echo "not nonconstant: 0.0")



(define-fun f ((x Real)) Real 0.0 )

(declare-fun x () Real)
(declare-fun cdgp.x () Real)

(assert (not
  (distinct (f x) (f cdgp.x))
))

(check-sat)
(get-model)
