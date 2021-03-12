; UCLID TEST
; Result=Some(true)
; Solver=z3
; Option=print-features

(declare-datatypes ((record 0)) ( ( (rec (fname String) (lname String) (id Int)) ) ) )
(declare-const x record)
(assert (and (= (fname x) "John") (= (lname x) "Smith")))
(check-sat)