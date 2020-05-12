package com.myspace.zio

import java.io.IOException

import scalaz.zio.{App, IO, Queue}
import scalaz.zio.console._

object ZioTest extends App {
  override def run(args: List[String]): IO[Nothing, ExitStatus] =
    myRun(fib2(20))

  def myRun(io: IO[IOException, Unit]): IO[Nothing, ExitStatus] =
    io.attempt.map(_.fold(_ => 1, _ => 0)).map(ExitStatus.ExitNow(_))

  def myAppLogic: IO[IOException, Unit] =
    for {
      _ <- putStrLn("Hello! What's your name?")
      n <- getStrLn
      _ <- putStrLn(s"Hello $n, nice to meet you!")
    } yield ()

  def fiber1: IO[IOException, Unit] =
    for {
      q <- Queue.unbounded[String]
      w = q.take.flatMap(putStrLn).forever
      _ <- IO.forkAll(List(w))
      _ <- q.offer("Mona")
    } yield ()

  def fibers: IO[IOException, Unit] =
    for {
      queue <- Queue.unbounded[String]
      worker = queue.take.flatMap(putStrLn).forever
      worker10k = List.fill(10000)(worker)

      _ <- IO.forkAll(worker10k)
      _ <- queue.offer("Beer")
    } yield ()

  def fib1(n: Int): IO[IOException, Unit] = {
    def go(n: Int): IO[IOException, Int] = {
      if (n <= 1) {
        IO.point(n)
      } else {
        for {
          fiber1 <- go(n - 1).fork
          fiber2 <- go(n - 2).fork
          v2 <- fiber2.join
          v1 <- fiber1.join
        } yield v1 + v2
      }
    }

    for {
      f <- go(n)
      _ <- putStrLn(s"fib($n) = $f")
    } yield ()
  }

  def fib2(n: Int): IO[IOException, Unit] = {
    def go(n: Int): IO[IOException, Int] =
      if (n <= 1)
        IO.now(n)
      else
        go(n - 1).parWith(go(n - 2))(_ + _)

    for {
      f <- go(n)
      _ <- putStrLn(s"fib($n) = $f")
    } yield ()
  }

  def schedule = {
    import scala.concurrent.duration._
    import scalaz.zio.Schedule

    val upTo10 = Schedule.recurs(10)
    val jittered = Schedule.exponential(10.milliseconds).jittered
  }
}

