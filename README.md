# First-Class Modules in Uclid5

This is a fork of [Uclid5](https://github.com/uclid-org/uclid) that
accommodates first-class modules and thus significantly changes the internal
representation. This work is possible thanks to these
[contributors](https://github.com/uclid-org/uclid/blob/master/CONTRIBUTORS.md),
and all code is covered by [this
license](https://github.com/uclid-org/uclid/blob/master/LICENSE). The goal is
ultimately to combine this fork with the main repository. If you use either
version of Uclid5 in your work, please cite [the original Uclid5 MEMOCODE'18
paper](https://cse.iitk.ac.in/users/spramod/papers/memocode18.pdf).

```
@inproceedings{seshia18uclid5,
  author    = {Sanjit A. Seshia and Pramod Subramanyan},
  title     = {{UCLID5:} Integrating Modeling, Verification, Synthesis and Learning},
  booktitle = {16th {ACM/IEEE} International Conference on Formal Methods and Models
               for System Design ({MEMOCODE})},
  pages     = {1--10},
  publisher = {{IEEE}},
  year      = {2018},
  doi       = {10.1109/MEMCOD.2018.8556946},
  location  = {Beijing, China}
}
```

Beyond first-class modules, this fork also carefully realizes the synthesis
encoding described in [this SYNT '20 workshop
paper](https://arxiv.org/abs/2007.06760).

# Usage

## Dependencies

- [Install SBT](https://www.scala-lang.org/download/)
- Install at least one of
  - [Alt-Ergo](https://alt-ergo.ocamlpro.com/)
  - [CVC4](https://github.com/CVC4/CVC4) (REQUIRED FOR SYNTHESIS)
  - [Vampire](https://github.com/vprover/vampire)
  - [Z3](https://github.com/Z3Prover/z3) (DEFAULT)

## Compile

Just add the bin folder to your path: uclidfc will automatically compile the
first time you run it. Note, uclidfc will not automatically recompile if you
make changes to its source code. To recompile, run `sbt assembly`.

## Run

```
uclidfc [options] <file> ...

Basic Usage
  -m, --main <module>            Name of the main module.
  -s, --solver <solver>          Solver to use (alt_ergo or cvc4 or vampire or z3). Solver must be in your path.
  -t, --timeout <timeout>        Timeout (in seconds) to give the solver.
  -o, --optimize <level>         Optimization level (0 or 1).
  -w, --write <file>             Write query to <file>.
  --pretty-print                 Try to make output queries human readable.
  --debug-print                  Add internal term graph information as SMT comments.
  --skip-solver                  Don't run the solver.
  --single-thread                Don't run solvers in parallel.
  <file> ...                     List of input files.

Analysis
  --print-features               Print query features.

Script Rewrites
  --assertion-over-conjunction   Rewrite asserted conjunctions to repeated assertions.

Arithmetic Rewrites
  --plus-minus-zero              Remove zeros from additions/subtractions.

Algebraic Datatype Rewrites
  --blast-enum-quantifiers       Rewrite quantifiers over enums to finite disjunctions/conjunctions.

String Rewrites
  --contains-over-concat         Rewrite "xy contains c," where c is a literal string of length 1.
  --contains-over-replace        Rewrite "(replace c1 with c2 in x) contains c3," where c1, c2, and c3 are literal strings of length 1.
  --indexof-gte-zero-gadgets     Rewrite "index of y in x >= 0" to "x contains y."


```

In SMT mode (when you call uclidfc on .smt2 files) uclidfc will iterate over the
files. If you give uclidfc multiple solver arguments, uclidfc will automatically
select the solver to use. So, the following command will, for each query in
`models/tests/smt2/`, print the query features, and use either z3 or cvc4 to
solve the query.

```
uclidfc models/tests/smt2/* --print-features -s cvc4 -s z3
```

## Live Edit

```
# depends on fswatch (`brew install fswatch`)
uclidfc-live <uclid files to edit> <query file to watch>
```

# Develop

### Check Coverage

```
sbt jacoco
open target/scala-3.0.0-RC1/jacoco/report/html/index.html
```

### Run JVM

```
sbt "run [options] <file> ..."
```