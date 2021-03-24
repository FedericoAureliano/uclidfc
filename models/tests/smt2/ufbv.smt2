; UCLID TEST
; Result=None
; Option=print-features
; Option=pretty-print

(set-info :smt-lib-version 2.6)
(set-logic UFBV)
(set-info :source |
Hardware fixpoint check problems.
These benchmarks stem from an evaluation described in Wintersteiger, Hamadi, de Moura: Efficiently solving quantified bit-vector formulas, FMSD 42(1), 2013.
The hardware models that were used are from the VCEGAR benchmark suite (see www.cprover.org/hardware/).
|)
(set-info :category "industrial")
(set-info :status unsat)
(declare-fun Verilog__main.a_64_0_39_!0 ((_ BitVec 2501) (_ BitVec 2501) (_ BitVec 2501) (_ BitVec 2501)) (_ BitVec 2501))
(declare-fun Verilog__main.b_64_0_39_!1 ((_ BitVec 2501) (_ BitVec 2501) (_ BitVec 2501) (_ BitVec 2501)) (_ BitVec 2501))
(assert (forall ((Verilog__main.a_64_0 (_ BitVec 2501)) (Verilog__main.b_64_0 (_ BitVec 2501)) (Verilog__main.a_64_1 (_ BitVec 2501)) (Verilog__main.b_64_1 (_ BitVec 2501))) (=> (and (= Verilog__main.a_64_0 (_ bv1 2501)) (= Verilog__main.b_64_0 (_ bv0 2501)) (= Verilog__main.a_64_1 (ite (bvult Verilog__main.a_64_0 (_ bv100 2501)) (bvadd Verilog__main.b_64_0 Verilog__main.a_64_0) Verilog__main.a_64_0)) (= Verilog__main.b_64_1 Verilog__main.a_64_0)) (and (= (Verilog__main.a_64_0_39_!0 Verilog__main.b_64_1 Verilog__main.a_64_1 Verilog__main.b_64_0 Verilog__main.a_64_0) (_ bv1 2501)) (= (Verilog__main.b_64_0_39_!1 Verilog__main.b_64_1 Verilog__main.a_64_1 Verilog__main.b_64_0 Verilog__main.a_64_0) (_ bv0 2501)) (= Verilog__main.a_64_1 (Verilog__main.a_64_0_39_!0 Verilog__main.b_64_1 Verilog__main.a_64_1 Verilog__main.b_64_0 Verilog__main.a_64_0)) (= Verilog__main.b_64_1 (Verilog__main.b_64_0_39_!1 Verilog__main.b_64_1 Verilog__main.a_64_1 Verilog__main.b_64_0 Verilog__main.a_64_0)))) ))
(check-sat)
(exit)