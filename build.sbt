name := "TubeUtil"

organization := "com.roundeights"

version := "0.1"

scalaVersion := "2.10.3"

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-feature")

// Application dependencies
libraryDependencies ++= Seq(
    "com.roundeights" %% "skene" % "0.1",
    "com.roundeights" %% "scalon" % "0.1",
    "com.roundeights" %% "hasher" % "1.0.0",
    "com.github.jknack" % "handlebars" % "1.0.0" % "provided",
    "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided",
    "org.specs2" %% "specs2" % "1.13" % "test",
    "org.mockito" % "mockito-all" % "1.9.5" % "test"
)

