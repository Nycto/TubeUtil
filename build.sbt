name := "TubeUtil"

organization := "com.roundeights"

version := "0.3.1"

scalaVersion := "2.11.7"

// append -deprecation to the options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-feature")

publishTo := Some("Spikemark" at "https://spikemark.herokuapp.com/maven/roundeights")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

resolvers ++= Seq("RoundEights" at "http://maven.spikemark.net/roundeights")

// Application dependencies
libraryDependencies ++= Seq(
    "com.roundeights" %% "skene" % "0.+",
    "com.roundeights" %% "scalon" % "0.+",
    "com.roundeights" %% "hasher" % "1.+",
    "com.roundeights" %% "isred" % "0.+" % "provided",
    "com.github.jknack" % "handlebars" % "2.+" % "provided",
    "org.slf4j" % "slf4j-simple" % "1.+",
    "javax.servlet" % "javax.servlet-api" % "3.0.+" % "provided",
    "org.specs2" %% "specs2" % "2.3.+" % "test"
)

