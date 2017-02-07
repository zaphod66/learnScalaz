package com.myspace.task

import scala.concurrent.ExecutionContext
import scalaz.Nondeterminism
import scalaz.concurrent.{Strategy, Task}

object TaskTest extends App {
  val t1 = Task.now(42)
  val t2 = Task.delay(43)
  val t3 = Task.fail(new Exception("oops"))

  val pool = ExecutionContext.fromExecutorService(Strategy.DefaultExecutorService)

  val t4 = Task.fork(t1)(pool)
  val t5 = Nondeterminism[Task].choose(t1, t2)
  val t6 = Nondeterminism[Task].both(t1, t2)

  def f(i: Int): Task[Int] = Task.delay(i + 1)

  val t7 = t1 flatMap f

  val prog = for {
    _ <- Task.delay(println("What's your name?"))
    s <- Task.delay(scala.io.StdIn.readLine())
    _ <- Task.delay(println(s"Hello $s"))
  } yield ()

  val i7 = t7.run

  prog.run
}
