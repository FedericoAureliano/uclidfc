sbt scalafmtAll
sbt fullOptJS; cp js/target/scala-2.13/uclid-opt.js docs/js/uclid-opt.js
sbt jacoco