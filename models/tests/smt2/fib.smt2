; UCLID TEST
; Solver=z3
; Result=Some(true)
; Option=print-features
; Option=pretty-queries

(set-info :smt-lib-version 2.6)
(set-info :category "industrial")
(set-info :source |Generator: Uclid5.|)
(set-info :status unknown)
(set-option :produce-assignments true)
(set-option :produce-models true)
; declaring module main
  (declare-datatypes ((main 0)) (((main
    (a Int)
    (b Int)))))
  (define-fun line12col5!11 ((State main)) main
    (main
      ; assigning to a
      (a State)
      ; assigning to b
      1))
  (define-fun line11col5!7 ((State main)) main
    (main
      ; assigning to a
      0
      ; assigning to b
      (b State)))
  (define-fun main!init ((State main)) main
    (line12col5!11 (line11col5!7 State)))
  (define-fun line17col5!51 ((State main)) main
    (main
      ; assigning to a
      (a State)
      ; assigning to b
      (+
        (a State)
        (b State))))
  (define-fun line16col5!49 ((State main)) main
    (main
      ; assigning to a
      (b State)
      ; assigning to b
      (b State)))
  (define-fun main!next ((State main)) main
    (line17col5!51 (line16col5!49 State)))
  (define-fun main!spec ((State main)) Bool
    (<=
      (a State)
      (b State)))
; done declaring module main

(declare-const fresh!1 main)
(declare-const fresh!2 main)
(assert (or
  (not (main!spec (main!init fresh!1)))
  (and
    (main!spec fresh!2)
    (not (main!spec (main!next fresh!2))))))
(check-sat)