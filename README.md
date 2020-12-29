# Uclid5 with First-Class Modules through Algebraic Data Types

## Dependencies

- [Install SBT](https://www.scala-lang.org/download/)
- [Install Z3](https://github.com/Z3Prover/z3)
- [Install CVC4](https://github.com/CVC4/CVC4)
- (Optional) fswatch (`brew install fswatch`)

## Users

### Compile

Just add the bin folder to your path: uclidfc will automatically compile the
first time you run it. Note, uclidfc will not automatically recompile if you make
changes to its source code.

### Run on JVM

```
uclidfc [options] <file> ...

  -m, --main <module>    Name of the main module.
  -s, --solver <solver>  Use <solver> (z3, cvc4)
  -r, --run <boolean>    Run the solver?
  -o, --out <file>       Write query to <file>.
  <file> ...             List of files to analyze.
```

### Live Edit

```
uclidfc-live <uclid files to edit> <query file to watch>
```

### Web & Docs

```
uclidfc-web
```

## Developers

### Check Format

```
sbt scalafmtSbtCheck scalafmtCheck test:scalafmtCheck
```

### Check Coverage

```
sbt jacoco
open jvm/target/scala-2.13/jacoco/report/html/index.html
```

### Run JVM

```
sbt "uclidJVM/run [options] <file> ..."
```

### Run JS

```
sbt fullOptJS; cp js/target/scala-2.13/uclidfc-opt.js docs/js/uclidfc-opt.js
open docs/index.html
```