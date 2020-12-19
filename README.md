# Trilingual Uclid

## Dependencies

- [Install SBT](https://www.scala-lang.org/download/)
- [Install Z3](https://github.com/Z3Prover/z3)
- (Optional) [Install CVC4](https://github.com/CVC4/CVC4)

## Users

### Compile

```
sbt assembly
```

### Run

```
./uclid [options] <file> ...

  -m, --main <Module>    Name of the main module.
  -s, --solver <Solver>  Path to solver.
  -p, --print            Print the query.
  <file> ...             List of files to analyze.
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
sbt fullOptJS; cp js/target/scala-2.13/uclid-opt.js docs/js/uclid-opt.js
open docs/index.html
```