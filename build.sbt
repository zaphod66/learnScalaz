name := """LearnScalaz"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.5"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

// val scalazVersion = "7.2.12"

val scalazVersion = "7.2.21"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
//  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "org.scalaz" %% "scalaz-ioeffect" % "2.1.0",
  "org.scalaz" %% "scalaz-zio" % "0.3.2",
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
  "com.storm-enroute" %% "scalameter-core" % "0.9",
  "com.storm-enroute" %% "scalameter" % "0.9" % "test"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.3"

// libraryDependencies += "org.scala-lang.modules" %% "scala-pickling" % "0.10.1"

// libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

// libraryDependencies += "com.typesafe.play" %% "play-json" % "2.4.2"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.26.5"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.2.1"

// libraryDependencies ++= Seq(slick, h2, scalaTest % Test)

libraryDependencies += "io.monix" %% "monix" % "2.3.3"

scalacOptions += "-feature"

scalacOptions += "-Ypartial-unification"

initialCommands in console := "import scalaz._, Scalaz._"
initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._, scalacheck.ScalaCheckBinding._"

