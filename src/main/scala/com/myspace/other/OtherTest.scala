package com.myspace.other

object OtherTest extends App {
  println("OtherTest")

  import PartialFunction._

  def strangeConditional(other: Any): Boolean = cond(other) {
    case x: String if x == "abc" || x == "def" => true
    case x: Int => true
  }

  def onlyInt(v: Any): Option[Int] = condOpt(v) { case x: Int => x }

}
