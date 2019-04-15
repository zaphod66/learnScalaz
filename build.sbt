name := """LearnScalaz"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.8"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

val scalazVersion = "7.2.8"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-typelevel" % "7.1.17",
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
  "com.storm-enroute" %% "scalameter-core" % "0.10.1",
  "com.storm-enroute" %% "scalameter" % "0.10.1" % "test"
)

libraryDependencies += "org.scalaz" %% "scalaz-zio" % "1.0-RC3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.3"

// libraryDependencies += "org.scala-lang.modules" %% "scala-pickling" % "0.10.1"

// libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

// libraryDependencies += "com.typesafe.play" %% "play-json" % "2.4.2"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.26.5"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.2.2"

libraryDependencies += "com.slamdata" %% "matryoshka-core" % "0.18.3"

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"
initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._, scalacheck.ScalaCheckBinding._"

