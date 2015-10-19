lazy val root =
  project.in(file(".")).
  aggregate(coreJVM, coreJS).
  settings(publish := {}, publishLocal := {})

lazy val core = crossProject.in(file(".")).
  settings(
    name := "opensolid-core",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).
  jvmSettings().
  jsSettings(scalaJSStage := FullOptStage)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js
