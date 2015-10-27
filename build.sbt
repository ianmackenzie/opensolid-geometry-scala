lazy val root =
  project.in(file(".")).
  aggregate(coreJVM, coreJS).
  settings(publish := {}, publishLocal := {})

lazy val core = crossProject.in(file(".")).
  settings(
    name := "opensolid-core",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
  ).
  jvmSettings(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.12.2" % "test",
    javacOptions += "-Xmx2048M",
    testFrameworks += new TestFramework("org.scalacheck.ScalaCheckFramework")
  ).
  jsSettings(scalaJSStage := FastOptStage)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js
