# UCLID5 v1.0

## Dependencies

[Install SBT](https://www.scala-lang.org/download/)

## Run JVM

```
sbt "uclidJVM/run examples/fib.ucl"
```

## Run JS

```
sbt fullOptJS; cp js/target/scala-2.13/uclid-opt.js docs/js/uclid-opt.js
open docs/index.html
```

## Compile

```
sbt assembly
```

## Run Jar

```
./uclid examples/fib.ucl
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