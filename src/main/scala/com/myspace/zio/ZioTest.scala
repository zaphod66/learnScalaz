package com.myspace.zio

import java.io.IOException

import scalaz.zio.{App, DefaultRuntime, ZIO}
import scalaz.zio.console._

object ZioTest extends App {

  override def run(args: List[String]): ZIO[ZioTest.Environment, Nothing, Int] =
    myAppLogic.either.map(_.fold(_ => 1, _ => 0))

  val myAppLogic: ZIO[Console, IOException, Unit] =
    for {
      _ <- putStrLn("Hello! What is your name?")
      n <- getStrLn
      _ <- putStrLn(s"Hello, $n, good to meet you!")
    } yield ()
}

object ZioWithoutRuntime extends scala.App {
  import ZioTest.myAppLogic

  val runtime = new DefaultRuntime {}

  runtime.unsafeRun(myAppLogic)
}