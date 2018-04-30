package com.myspace.writer

import scalaz._, Scalaz._

object MyWriter {
  case class Writer[W, A](w: W, a: A)

  type Log = List[String]

  def toDouble(s: String): Writer[Log, Double] = Writer(List(s"""toDouble(s"$s")"""), s.toDouble)
}

object WriterTest extends App {
  def gcd(a: Int, b: Int): Writer[List[String], Int] = {
    if (b == 0)
      for { _ <- List("finished with " + a.shows).tell } yield a
    else
      List(s"${a.shows} mod ${b.shows} = ${(a % b).shows}").tell >>= { _ =>
        gcd(b, a % b)
      }
  }

  val writer1 = gcd(1124, 160)
  val result1 = writer1.run

  println(s"""steps:  ${result1._1.mkString(" | ")}""")
  println(s"""result: ${result1._2}""")

  val w = MyWriter.toDouble("42")

  println(s"result: ${w.a}")
  println(s"log:    ${w.w.mkString}")
}
