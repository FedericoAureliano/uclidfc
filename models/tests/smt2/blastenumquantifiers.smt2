; UCLID TEST
; Solver=z3
; Result=Some(true)
; Rewrite=blast-enum-quantifiers
; Option=print-features

(declare-datatypes ((list 0)) 
    (((cons (head Int) (tail list)) (nil))))

(declare-datatypes ((Color 0)) 
    (((Red) (Black))))

(declare-datatypes ((record 0))
    (((rec (fname String) (lname String) (id Int)))))
(declare-const z record)
(assert (and (= (fname z) "John") (= (lname z) "Smith")))

(assert (forall ((k Color)) (or (= k Red) (= k Black))))

(check-sat)

(declare-const x Int)
(declare-const y Int)
(declare-fun f (Int Int) Int)
(declare-fun g () Bool)
(assert (= (- (f x y)) (+ x (- y) 1)))
(assert (exists ((l Int)(k Int)) (and (> (+ l k) 0) (< (- l k) 0))))
(check-sat)