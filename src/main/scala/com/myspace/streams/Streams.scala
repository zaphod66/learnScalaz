package com.myspace.streams

import scala.concurrent.Future
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import scala.util.{Failure, Success}
import scala.util.matching.Regex

object Streams extends App {

  val t1 = Task.delay(scala.io.StdIn.readLine())  // Task[String]
  val t2 = Task.now(42)                           // Task[Int]
  val t3 = Task.fail(new Exception("oops!"))      // Task[Nothing]

  {
    import scala.concurrent.ExecutionContext.Implicits.global

    val f1 = Future { 42 }
    val t4 = Task.async[Int] { k => f1.onComplete {
      case Success(a) => k(\/.right(a))
      case Failure(e) => k(\/.left(e))
      }
    }

    val pool = java.util.concurrent.Executors.newSingleThreadExecutor
    val t5 = Task.fork(t4)(pool)

    val t6 = Nondeterminism[Task].both(t2, t4)    // Task[(Int, Int)] // running both concurrently
  }

  val prog: Task[Unit] = for {
    _ <- Task.delay(println("What's your name?"))
    n <- Task.delay(scala.io.StdIn.readLine())
    _ <- Task.delay(println(s"Hello $n"))
  } yield ()

//val p1 = prog.run

  val v1 = t3.attempt

  // Process (will be renamed to Stream in the next iteration)

  val p1 = Process.halt
  val p2 = Process.emit(42)
//val p3 = Process.await()

  val p5 = Process.eval(Task.delay(scala.io.StdIn.readLine()))

  val p6 = p2 ++ p2

  def IO[A](a: => A): Process[Task, A] = Process.eval(Task.delay(a))

  val twoLines: Process[Task, String] = IO(scala.io.StdIn.readLine()) ++ IO(scala.io.StdIn.readLine())

  val stdIn1: Process[Task, String] = IO(scala.io.StdIn.readLine()) ++ stdIn1
  val stdIn2: Process[Task, String] = IO(scala.io.StdIn.readLine()).repeat

  val cat1: Process[Task, Unit] = stdIn1 flatMap { s =>
    IO(println(s))
  }
  val cat2: Process[Task, Unit] = for {
    s <- stdIn1
    _ <- IO(println(s))
  } yield ()

  def grep(r: Regex): Process[Task, Unit] = {
    val p = r.pattern.asPredicate.test _
    def out(s: String) = IO(println(s))

    stdIn1 filter p flatMap out
  }

  // Pipes

  def take[I](n: Int): Process1[I, I] =
    if (n <= 0) Process.halt
    else Process.await1[I] ++ take(n - 1)

  val chunker = process1.chunk[Int](10)

  val c1 = p2 pipe chunker

  def distinct[A]: Process1[A, A] = {
    def go(seen: Set[A]): Process1[A, A] =
      Process.await1[A].flatMap { a =>
        if (seen(a)) go(seen)
        else Process.emit(a) ++ go(seen + a)
      }
    go(Set.empty)
  }

  // Multiple sources

  import scalaz.stream.tee._

  val r1 = scalaz.stream.io.linesR("/tmp/foo.txt")
  val r2 = scalaz.stream.io.linesR("/tmp/bar.txt")

  type Source[A] = Process[Task, A]

  val s1 = r1 zip r2                    // Source[(String, String)]
  val s2 = r1 interleave r2             // Source[String]
  val s3 = r1 until r2.map(_ == "stop") // Source[String]

  // s1 is syntactic sugar for f1.tee(f2)(tee.zip) ...

  val add: Tee[Int, Int, Int] = {
    for {
      x <- Process.awaitL[Int]
      y <- Process.awaitR[Int]
    } yield x + y
  }.repeat

  val p7 = Process.range(10, 20)
  val p8 = Process.range(20, 30)

  val sumEach = (p7 tee p8)(add)

}
