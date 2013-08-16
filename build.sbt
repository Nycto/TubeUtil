name := "TubeUtil"

organization := "com.roundeights"

version := "0.1"

scalaVersion := "2.10.1"

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-feature")

// Application dependencies
libraryDependencies ++= Seq(
    "com.roundeights" %% "skene" % "0.1"
)

