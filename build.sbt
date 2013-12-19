name := "TubeUtil"

organization := "com.roundeights"

version := "0.2"

scalaVersion := "2.10.3"

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-feature")

publishTo := Some("Spikemark" at "https://spikemark.herokuapp.com/maven/roundeights")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

resolvers ++= Seq("RoundEights" at "http://maven.spikemark.net/roundeights")

// Application dependencies
libraryDependencies ++= Seq(
    "com.roundeights" %% "skene" % "0.2.0",
    "com.roundeights" %% "scalon" % "0.1",
    "com.roundeights" %% "hasher" % "1.0.0",
    "com.roundeights" %% "isred" % "0.2" % "provided",
    "com.github.jknack" % "handlebars" % "1.0.0" % "provided",
    "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided",
    "org.specs2" %% "specs2" % "2.3.4" % "test"
)

