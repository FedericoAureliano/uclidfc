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
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    jacocoReportSettings := JacocoReportSettings(
      "Jacoco Coverage Report",
      None,
      JacocoThresholds(
        instruction = 80,
        method = 100,
        branch = 100,
        complexity = 100,
        line = 90,
        clazz = 100
      ),
      Seq(JacocoReportFormats.ScalaHTML, JacocoReportFormats.XML),
      "utf-8"
    )
  )
  .jvmSettings(
    // JVM-specific settings
    libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1"
  )
  .jsSettings(
    // JS-specific settings
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
  )
