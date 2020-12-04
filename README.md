# UCLID5 v1.0

## Dependencies

[Install SBT](https://www.scala-lang.org/download/)

## JVM

```
sbt "uclidJVM/run examples/fib.ucl"
```

## JS

```
sbt fullOptJS; cp js/target/scala-2.13/uclid-opt.js docs/js/uclid-opt.js
open docs/index.html
```