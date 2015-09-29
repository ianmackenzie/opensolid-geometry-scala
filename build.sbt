lazy val root =
  project.in(file(".")).
  aggregate(opensolidJS, opensolidJVM).
  settings(publish := {}, publishLocal := {})

lazy val opensolid = crossProject.in(file(".")).
  settings(
    name := "opensolid",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).
  jvmSettings().
  jsSettings(
    scalaJSStage := FullOptStage,
    postLinkJSEnv := NodeJSEnv().value
  )

lazy val opensolidJVM = opensolid.jvm

lazy val opensolidJS = opensolid.js
