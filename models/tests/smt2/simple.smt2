; UCLID TEST
; Solver=z3
; Result=Some(true)

(declare-const x Int)
(declare-const y Int)
(declare-fun f (Int Int) Int)
(declare-fun g () Bool)
(assert (= (- (f x y)) (+ x (- y) 1)))
(assert (exists ((l Int)(k Int)) (and (> (+ l k) 0) (< (- l k) 0))))
(check-sat)