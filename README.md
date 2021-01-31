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

## Run on JVM

```
uclidfc [options] <file> ...

  -m, --main <module>         Name of the main module.
  -s, --solver <solver>       Use one of alt_ergo or cvc4 or vampire or z3. Solver must be in your path.
  -o, --out <file>            Write query to <file>.
  --skip-solver               Don't run the solver.
  --print-features            Print query features.
  --blast-enum-quantifiers    Rewrite quantifiers over enums to finite disjunctions/conjunctions.
  <file> ...                  List of files to analyze.
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
open jvm/target/scala-2.13/jacoco/report/html/index.html
```

### Run JVM

```
sbt "run [options] <file> ..."
```