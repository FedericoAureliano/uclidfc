; UCLID TEST
; Result=None
; Option=print-features
; Option=pretty-print

(set-info :smt-lib-version 2.6)
(set-logic BV)
(set-info :source |
These benchmarks used in the paper:

  Dejan Jovanovic and Leonardo de Moura.  Solving Non-Linear Arithmetic.
  In IJCAR 2012, published as LNCS volume 7364, pp. 339--354.

The keymaera family contains VCs from Keymaera verification, see:

  A. Platzer, J.-D. Quesel, and P. Rummer.  Real world verification.
  In CADE 2009, pages 485-501. Springer, 2009.

Submitted by Dejan Jovanovic for SMT-LIB.

 KeYmaera example: binary_driver-2007-10-09, node 12552 For more info see: No further information available.

Translated to BV by Mathias Preiner.
|)
(set-info :license "https://creativecommons.org/licenses/by/4.0/")
(set-info :category "industrial")
(set-info :status sat)
(declare-fun vdesuscore2dollarskuscore6 () (_ BitVec 32))
(declare-fun d () (_ BitVec 32))
(declare-fun vuscore2dollarskuscore7 () (_ BitVec 32))
(declare-fun b () (_ BitVec 32))
(declare-fun ts6uscore0 () (_ BitVec 32))
(declare-fun duscore2dollarskuscore7 () (_ BitVec 32))
(declare-fun stateuscore2dollarskuscore4 () (_ BitVec 32))
(declare-fun m () (_ BitVec 32))
(declare-fun v () (_ BitVec 32))
(declare-fun t6uscore0dollarskuscore0 () (_ BitVec 32))
(declare-fun zuscore2dollarskuscore7 () (_ BitVec 32))
(declare-fun ep () (_ BitVec 32))
(declare-fun muscore2dollarskuscore7 () (_ BitVec 32))
(declare-fun z () (_ BitVec 32))
(declare-fun amax () (_ BitVec 32))
(assert (not (exists ((ts6uscore0 (_ BitVec 32))) (let ((?v_4 (bvneg b)) (?v_0 (bvadd (bvsdiv amax b) (_ bv1 32))) (?v_2 (bvmul (_ bv2 32) b)) (?v_6 (bvmul duscore2dollarskuscore7 duscore2dollarskuscore7))) (let ((?v_1 (bvsub (bvmul vuscore2dollarskuscore7 vuscore2dollarskuscore7) ?v_6)) (?v_3 (bvsub muscore2dollarskuscore7 zuscore2dollarskuscore7)) (?v_5 (bvadd (bvmul ?v_4 t6uscore0dollarskuscore0) vuscore2dollarskuscore7))) (=> (and (and (and (and (and (and (and (and (and (and (and (and (and (=> (and (bvsle (_ bv0 32) ts6uscore0) (bvsle ts6uscore0 t6uscore0dollarskuscore0)) (and (bvsge (bvadd (bvmul ?v_4 ts6uscore0) vuscore2dollarskuscore7) (_ bv0 32)) (bvsle ts6uscore0 ep))) (bvsge t6uscore0dollarskuscore0 (_ bv0 32))) (bvsgt ?v_3 (bvadd (bvadd (bvmul (bvmul ?v_0 ep) vuscore2dollarskuscore7) (bvsdiv ?v_1 ?v_2)) (bvsdiv (bvmul (bvmul ?v_0 amax) (bvmul ep ep)) (_ bv2 32))))) (not (= stateuscore2dollarskuscore4 (_ bv1 32)))) (bvsge vuscore2dollarskuscore7 vdesuscore2dollarskuscore6)) (bvsle ?v_1 (bvmul ?v_2 ?v_3))) (bvsge vuscore2dollarskuscore7 (_ bv0 32))) (bvsge duscore2dollarskuscore7 (_ bv0 32))) (bvsle (bvsub (bvmul v v) (bvmul d d)) (bvmul ?v_2 (bvsub m z)))) (bvsge v (_ bv0 32))) (bvsgt ep (_ bv0 32))) (bvsgt b (_ bv0 32))) (bvsgt amax (_ bv0 32))) (bvsge d (_ bv0 32))) (bvsle (bvsub (bvmul ?v_5 ?v_5) ?v_6) (bvmul ?v_2 (bvsub muscore2dollarskuscore7 (bvmul (bvsdiv (_ bv1 32) (_ bv2 32)) (bvadd (bvadd (bvmul ?v_4 (bvmul t6uscore0dollarskuscore0 t6uscore0dollarskuscore0)) (bvmul (bvmul (_ bv2 32) t6uscore0dollarskuscore0) vuscore2dollarskuscore7)) (bvmul (_ bv2 32) zuscore2dollarskuscore7))))))))))))
(check-sat)
(exit)
