package com.myspace.other

import scalaz._
import Scalaz._

case class Item(stuff: Int)
case class Result(successes: Int, failures: Int)

object MonoidTest extends App {
  implicit val resultMonoid = new Monoid[Result] {
    override def zero: Result = Result(0, 0)

    override def append(f1: Result, f2: => Result): Result = {
      Result(f1.successes + f2.successes, f1.failures + f2.failures)
    }
  }

  val l: List[Item] = List(Item(10), Item(12), Item(15))

  def processItem(item: Item): Result = {
    item match {
      case Item(i) => if (i <= 10) Result(10, 0) else Result(10, i - 10)
    }
  }

  val r = l.foldMap(processItem)

  println(s"result: $r")
}
