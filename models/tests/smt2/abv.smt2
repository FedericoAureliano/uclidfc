; UCLID TEST
; Result=None
; Option=debug-print

(set-info :smt-lib-version 2.6)
(set-logic ABV)
(set-info :source "|
Generated by the tool Ultimate Automizer [1,2] which implements
an automata theoretic approach [3] to software verification.

This SMT script belongs to a set of SMT scripts that was generated by
applying Ultimate Automizer to benchmarks [4] from the SV-COMP 2019 [5,6].
This script might _not_ contain all SMT commands that are used by
Ultimate Automizer. In order to satisfy the restrictions of
the SMT-COMP we have to drop e.g., the commands for getting
values (resp. models), unsatisfiable cores and interpolants.

2019-04-27, Matthias Heizmann (heizmann@informatik.uni-freiburg.de)

[1] https://ultimate.informatik.uni-freiburg.de/automizer/
[2] Matthias Heizmann, Yu-Fang Chen, Daniel Dietsch, Marius Greitschus,
     Jochen Hoenicke, Yong Li, Alexander Nutz, Betim Musa, Christian
     Schilling, Tanja Schindler, Andreas Podelski: Ultimate Automizer
     and the Search for Perfect Interpolants - (Competition Contribution).
     TACAS (2) 2018: 447-451
[3] Matthias Heizmann, Jochen Hoenicke, Andreas Podelski: Software Model
     Checking for People Who Love Automata. CAV 2013:36-52
[4] https://github.com/sosy-lab/sv-benchmarks
[5] Dirk Beyer: Automatic Verification of C and Java Programs: SV-COMP 2019.
     TACAS (3) 2019: 133-155
[6] https://sv-comp.sosy-lab.org/2019/
|")
(set-info :license "https://creativecommons.org/licenses/by/4.0/")
(set-info :category "industrial")
(set-info :status unknown)
(declare-fun ~unnamed0~0~P_ALL () (_ BitVec 32))
(declare-fun ~unnamed0~0~P_PID () (_ BitVec 32))
(declare-fun ~unnamed0~0~P_PGID () (_ BitVec 32))
(declare-fun |c_#valid| () (Array (_ BitVec 32) Bool))
(declare-fun |c_#memory_int| () (Array (_ BitVec 32) (Array (_ BitVec 32) (_ BitVec 32))))
(declare-fun c_main_~a~0.base () (_ BitVec 32))
(declare-fun c_main_~a~0.offset () (_ BitVec 32))
(declare-fun c_main_~p~0.base () (_ BitVec 32))
(declare-fun c_main_~p~0.offset () (_ BitVec 32))
(assert (= ~unnamed0~0~P_ALL (_ bv0 32)))
(assert (= ~unnamed0~0~P_PID (_ bv1 32)))
(assert (= (_ bv2 32) ~unnamed0~0~P_PGID))
(assert (and (select |c_#valid| c_main_~a~0.base) (or (forall ((v_DerPreprocessor_2 (_ BitVec 32))) (= (_ bv1 32) (select (select (store |c_#memory_int| c_main_~p~0.base (store (select |c_#memory_int| c_main_~p~0.base) (bvadd c_main_~p~0.offset (_ bv4 32)) v_DerPreprocessor_2)) c_main_~a~0.base) c_main_~a~0.offset))) (forall ((v_main_~t~0.base_7 (_ BitVec 32))) (select |c_#valid| v_main_~t~0.base_7)))))
(assert (not (and (not (= c_main_~p~0.base c_main_~a~0.base)) (= (_ bv1 32) (select (select |c_#memory_int| c_main_~a~0.base) c_main_~a~0.offset)))))
(check-sat)
(exit)