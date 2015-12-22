// Override sbt-codacy-coverage's dependency on version 1.1.2, which seems to no longer be
// available
libraryDependencies += "com.typesafe.netty" % "netty-http-pipelining" % "1.1.4"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.3")
addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.2.1")
addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")
