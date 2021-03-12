; UCLID TEST
; Solver=z3
; Result=Some(false)

(set-info :smt-lib-version 2.6)
(set-info :category "industrial")
(set-info :source |Generator: Uclid5.|)
(set-info :status unknown)
(set-option :produce-assignments true)
(declare-datatypes ((intholder 0)) (((intholder     (x Int)     (y Int)     (z Int))))) (declare-datatypes ((intholderholder 0)) (((intholderholder     (first intholder)     (second intholder))))) (declare-datatypes ((intholderholderholder 0)) (((intholderholderholder     (only intholderholder)))))   (declare-datatypes ((main 0)) (((main     (t intholderholderholder)))))   (define-fun line15col9!177 ((State main)) main     (main (intholderholderholder (intholderholder  (first (only (t State)))  (intholder  (x (second (only (t State))))  (y (second (only (t State))))  (- 66))))))   (define-fun line14col9!159 ((State main)) main     (main (intholderholderholder (intholderholder  (intholder  (- 66)  (y (first (only (t State))))  (z (first (only (t State)))))  (second (only (t State)))))))   (define-fun main!init ((State main)) main     (line15col9!177 (line14col9!159 State)))   (define-fun main!next ((State main)) main     State)   (define-fun main!spec ((State main)) Bool     (= (x (first (only (t State)))) (z (second (only (t State)))))) (declare-const fresh!1 main) (declare-const fresh!2 main)
(assert (! (or (! (not (! (main!spec (! (main!init (! fresh!1 :named fresh!7)) :named fresh!6)) :named fresh!5)) :named fresh!4) (! (and (! (main!spec (! fresh!2 :named fresh!10)) :named fresh!9) (! (not (! (main!spec (! (main!next fresh!10) :named fresh!13)) :named fresh!12)) :named fresh!11)) :named fresh!8)) :named fresh!3)) (check-sat)