package com.myspace.futures

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global

object FutureTest extends App {
  def f(n: Int): Int = n + 1

  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case n => fib(n - 2) + fib(n - 1)
  }

  def logDuration[T](label: String)(t: => T): T = {
    val start = System.nanoTime()
    val res   = t
    val end   = System.nanoTime()

    println(s"==> $label took ${(end-start)/1000000} millis")

    res
  }

  println("====")
  val fibs1 = logDuration("sequential")((1 to 10) map fib)
  fibs1 foreach { i => print(i + " ") }
  println
  println("====")

  val l1 = for {
    i <- 1 to 40
    fu = Future { fib(i) }
  } yield fu

  val f1 = Future.sequence(l1)

  Await.ready(f1, 200 seconds)

  f1 onSuccess { case l => print(l.mkString(" ")) }
//  f1 onComplete {
//    case Success(l) =>
//      println("-->" + l.mkString(" "))
//      println(s"--> ${l foreach { i => print(s"$i ")} }")
//    case Failure(e) => println(s"ERROR: ${e.getMessage}")
//  }

  Await.ready(f1, 200 seconds)

  println("====")
}
