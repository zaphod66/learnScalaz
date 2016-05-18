package com.myspace.other

object OtherTest extends App {
  println("OtherTest")

  import PartialFunction._

  def strangeConditional(other: Any): Boolean = cond(other) {
    case x: String if x == "abc" || x == "def" => true
    case x: Int => true
  }

  def onlyInt(v: Any): Option[Int] = condOpt(v) { case x: Int => x }

  val s1 = Seq(1,2,3,4,5)
  val s1p = s1.par
  val s1pt = s1p.tasksupport

  val t1 = Thing.point(1)
  val t2 = t1.flatMap(i => Thing.point(i + 1))

  println("=========")

  val t3 = for {
    i <- t1
  } yield i + 1

  println(s"t3 = $t3")

}

case class Thing(i: Int) {
  def map(f: Int => Int)       = { val t = f(i); println(s"map($i => $t)"); t }
  def flatMap(f: Int => Thing) = { val t = f(i); println(s"flatMap($i => $t)"); t }
}

object Thing {
  def point(i: Int) = { println(s"point($i)"); Thing(i) }
}