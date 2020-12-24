ThisBuild / scalaVersion := "2.13.3"

lazy val root = project
  .in(file("."))
  .aggregate(uclid.js, uclid.jvm)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val uclid = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "uclid",
    version := "1.0",
    javacOptions ++= Seq("-source", "1.6", "-target", "1.6"),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies += ("org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2")
      .withSources(),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
  .jvmSettings(
    // JVM-specific settings
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1",
    jacocoReportSettings := JacocoReportSettings(
      "Jacoco Coverage Report",
      Some("shared"),
      JacocoThresholds(
        instruction = 70,
        method = 50,
        branch = 40,
        complexity = 40,
        line = 90,
        clazz = 50
      ),
      Seq(JacocoReportFormats.ScalaHTML),
      "utf-8"
    )
  )
  .jsSettings(
    // JS-specific settings
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
  )
