val scala3Version = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "uclidfc",
    version := "1.0",

    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),

    libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-M1"),
    libraryDependencies += ("com.novocode" % "junit-interface" % "0.11" % "test"),
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0",
    jacocoReportSettings := JacocoReportSettings(
      "Jacoco Coverage Report",
      Some("shared"),
      JacocoThresholds(
        instruction = 65,
        method = 65,
        branch = 55,
        complexity = 60,
        line = 90,
        clazz = 90
      ),
      Seq(JacocoReportFormats.ScalaHTML),
      "utf-8"
    )
  )