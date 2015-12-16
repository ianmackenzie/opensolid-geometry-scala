lazy val root =
  project.in(file(".")).
  aggregate(coreJVM, coreJS).
  settings(publish := {}, publishLocal := {})

scalaSource in Compile := baseDirectory.value / "shared" / "src"

lazy val core = crossProject.in(file(".")).
  settings(
    name := "opensolid-core",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")
  ).
  jvmSettings(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.12.5" % "test",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "2.2.4" % "test",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    compileOrder in Test := CompileOrder.ScalaThenJava,
    logBuffered in Test := false,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
  ).
  jsSettings(scalaJSStage := FastOptStage)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js
