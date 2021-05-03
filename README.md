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
  -t, --timeout <timeout>        Timeout (in whole seconds) to give the solver per query.
  -w, --write <file>             Write query to <file>.
  --pretty-queries               Try to make output queries human readable.
  --debug-queries                Add internal term graph information as SMT comments.
  --skip-solver                  Don't run the solver.
  --single-thread                Don't run solvers in parallel.
  --table                        Print CSV table of results.
  <file> ...                     List of input files.

Analysis
  --print-features               Print query features.

Algebraic Datatype Rewrites
  --blast-enum-quantifiers       Rewrite quantifiers over enums to finite disjunctions/conjunctions.

Idiolect
  --language-models <folder>     Path to folder with idiolect models. Required for solver selection and training.
  --train                        Train solver idiolect models. Requires language-models folder and at least two solvers.

Utility
  --simulate <file>              Use the solver and query data in <file> to simulate solver execution.
```

In SMT mode (when you call uclidfc on .smt2 files) uclidfc will iterate over
the input files. If you give uclidfc multiple solver arguments and a data
folder with idiolect models, uclidfc will automatically select the solver to
use. So, the following command will, for each query in `models/tests/smt2/`,
print the query features, and use either z3 or cvc4 to solve the query with
a five second timeout per query.
```
uclidfc models/tests/smt2/* -s cvc4 -s z3 --language-models data -t 5 --table
```

The previous command assumes that there are language models in a folder called `data`. 
To train idiolect models and save them in a folder called `data`, do
```
uclidfc models/tests/smt2/* -s cvc4 -s z3 --language-models data --train -t 5 --table
```

Instead of running solvers on the same queries over and over, you can save
solver results and simulate their execution using the `--simulate` option.
To save results, just redirect stdout to a CSV file and use `--table`.
```
uclidfc models/tests/smt2/* -s cvc4 -s z3 --language-models data --train -t 5 --table > data.csv
```

To run with solver execution simulation, just point uclidfc to the data.csv file.
```
uclidfc models/tests/smt2/* -s cvc4 -s z3 --language-models data -t 5 --table --simulate data.csv
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