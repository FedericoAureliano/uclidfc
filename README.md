# UCLID5 v1.0

## Dependencies

[Install SBT](https://www.scala-lang.org/download/)

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
sbt "uclidJVM/run examples/fib.ucl"
```

### Run JS

```
sbt fullOptJS; cp js/target/scala-2.13/uclid-opt.js docs/js/uclid-opt.js
open docs/index.html
```