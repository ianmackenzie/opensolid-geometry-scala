enablePlugins(ScalaJSPlugin)

name := "OpenSolid"

scalaVersion := "2.11.7"

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

postLinkJSEnv := NodeJSEnv().value

scalaJSStage in Global := FastOptStage

testFrameworks += new TestFramework("utest.runner.Framework")
