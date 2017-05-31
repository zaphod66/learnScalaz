name := """LearnScalaz"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

val scalazVersion = "7.1.3"

val akkaVersion = "2.4.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka"  %% "akka-actor"                    % akkaVersion,
  "com.typesafe.akka"  %% "akka-persistence"              % akkaVersion,
  "com.typesafe.akka"  %% "akka-stream"                   % akkaVersion,
  "com.typesafe.akka"  %% "akka-persistence"              % akkaVersion,
  "com.typesafe.akka"         %%  "akka-persistence-query-experimental" % akkaVersion,
  "org.iq80.leveldb"          % "leveldb"                 % "0.7",
  "org.fusesource.leveldbjni" % "leveldbjni-all"          % "1.8",

  "commons-io"                %   "commons-io"                          % "2.4",

  "com.typesafe.scala-logging" %% "scala-logging-slf4j"   % "2.1.2",
  "ch.qos.logback"   % "logback-classic"                  % "1.1.3",
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test",
  "com.storm-enroute" %% "scalameter-core" % "0.6",
  "com.storm-enroute" %% "scalameter" % "0.6" % "test"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.3"

// libraryDependencies += "org.scala-lang.modules" %% "scala-pickling" % "0.10.1"

// libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

// libraryDependencies += "com.typesafe.play" %% "play-json" % "2.4.2"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.25.0"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.1-M4"

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"
initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._, scalacheck.ScalaCheckBinding._"

