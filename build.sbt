name := """LearnScalaz"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

val scalazVersion = "7.1.0"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz.stream" %% "scalaz-stream" % "0.7a",
  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
  "org.scala-lang.modules" %% "scala-pickling" % "0.10.1"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.3"

// libraryDependencies += "com.typesafe.play" %% "play-json" % "2.4.2"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.25.0"

// libraryDependencies += "io.argonaut" %% "argonaut" % "6.0.4"

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"

