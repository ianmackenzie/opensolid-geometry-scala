lazy val root = project.in(file(".")).
  settings(
    name := "opensolid-core",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings"),
    licenses += ("MPL-2.0", url("https://www.mozilla.org/en-US/MPL/2.0/")),
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    libraryDependencies += "net.bytebuddy" % "byte-buddy" % "0.7.7",
    logBuffered in Test := true,
    bintrayVcsUrl := Some("git@github.com:ianmackenzie/opensolid-core.git")
  )

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "https://github.com/ianmackenzie/opensolid-core.git"
