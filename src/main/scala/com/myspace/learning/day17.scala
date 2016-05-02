package com.myspace.learning

import scalaz._, Scalaz._
import effect._, IO._

object day17 extends App {

  val action1 = for {
    _ <- putStrLn("Hello, world!")
  } yield ()

  action1.unsafePerformIO

  val action2 = IO {
    val source = scala.io.Source.fromFile("./build.sbt")
    source.getLines().toStream
  }

  val l1 = action2.unsafePerformIO().toList

  def prog: IO[Unit] = for {
    line <- readLn
    _    <- putStrLn(line)
  } yield ()
}
