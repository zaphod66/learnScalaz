package com.myspace.futures

import java.util.concurrent.Executors

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.duration.Duration

object FutureSequence extends App {
  import scala.concurrent._

  private def futureSequence(title: String)(implicit exec: ExecutionContext): Unit = {
    val f1 = Future {
      1
    }

    val f2 = Future {
      2
    }

    val lf = List(f1, f2)

    val seq = Future.sequence(lf)

    seq.onSuccess {
      case l => println(s"$title: $l")
    }
  }

  object ImplicitContext {
    import ExecutionContext.Implicits.global

    futureSequence("Implicit")
  }

  object ExplicitContext {
    import java.util.concurrent.Executors

    implicit val exec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

    futureSequence("Explicit")
  }

  object CanBuild {
    val cbf = new CanBuildFrom[List[Future[Int]], Int, List[Int]] {
      def mb = new mutable.Builder[Int, List[Int]] {
        val buffer = new mutable.ListBuffer[Int]()

        override def +=(elem: Int): this.type = {
          println(s"+=($elem)")
          buffer.+=(elem); this
        }

        override def clear(): Unit = {
          println("clear()")
          buffer.clear()
        }

        override def result(): List[Int] = {
          println("result()")
          buffer.toList
        }
      }

      override def apply(from: List[Future[Int]]): mutable.Builder[Int, List[Int]] = {
        println(s"apply($from)")
        mb
      }
      override def apply(): mutable.Builder[Int, List[Int]] = {
        println("apply()")
        mb
      }
    }

    val exec = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

    val f0 = Future { Thread.sleep(1000); 0 } (exec)
    val f3 = Promise.successful(3).future
    val f4 = Promise.successful(4).future
    val f5 = Future { Thread.sleep(2000); 5 } (exec)

    val lf = List(f0, f3, f4, f5)

    val seq = Future.sequence(lf)(cbf, exec)

    val l = Await.result(seq, Duration.Inf)

    println(s"CanBuild: $l")
  }

  ImplicitContext
  ExplicitContext
  CanBuild
}
