package com.myspace.implicits

import scalaz.{Monoid, Semigroup}

trait Compute[A] {
  type Out

  def compute(a: A): Out
}

object Compute {
  implicit val int = new Compute[Int] {
    type Out = Double

    override def compute(a: Int): Double = 1.0 / a
  }

  implicit val string = new Compute[String] {
    type Out = Option[Char]

    override def compute(s: String): Option[Char] = s.headOption
  }
}

object ImplicitComputeTest extends App {
  def go[A](a: A)(implicit A: Compute[A]): A.Out = {
    A.compute(a)
  }

  val r1 = go(42)
  val r2 = go("Hello")

  println(s"r1: $r1")
  println(s"r2: $r2")
}
