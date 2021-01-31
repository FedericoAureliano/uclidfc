; UCLID TEST
; Solver=z3
; Result=Some(false)

(set-logic QF_LIA)
(declare-const x Int)
(declare-const y Int)
(assert (= (- x y) (+ x (- y) 1)))
(check-sat)