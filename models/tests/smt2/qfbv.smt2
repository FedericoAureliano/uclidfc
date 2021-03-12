; UCLID TEST
; Solver=z3
; Result=Some(true)
; Option=print-features
; Option=pretty-print

(define-sort TEST () Int)
(declare-const f TEST)

(declare-fun main_~i~0 () (_ BitVec 32))
(declare-fun main_~sn~0 () (_ BitVec 32))
(declare-fun main_~n~0 () (_ BitVec 32))
(assert (or (= (_ bv0 32) main_~sn~0) (= (bvudiv (bvmul main_~n~0 (bvadd main_~n~0 (_ bv1 32))) (_ bv2 32)) main_~sn~0)))
(check-sat)
