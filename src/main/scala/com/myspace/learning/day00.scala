package com.myspace.learning

object day00 extends App {

  println("learning Scalaz day00")

  trait Plus[A] {
    def plus(a1: A, a2: A): A
  }

  def plus[A: Plus](a1: A, a2: A): A = implicitly[Plus[A]].plus(a1, a2)

  implicit val plusInt = new Plus[Int] {
    override def plus(a1: Int, a2: Int): Int = a1 + a2
  }

  implicit val plusStr = new Plus[String] {
    override def plus(a1: String, a2: String): String = a1 + a2
  }

  val bi = plus(1, 2)
  val bs = plus("a", "b")

  println(s"bi = $bi")
  println(s"bs = $bs")
}
