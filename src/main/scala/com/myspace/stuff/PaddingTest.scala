package com.myspace.stuff

import scalaz._, Scalaz._

object PaddingTest extends App {
  val s = List(List("11", "222", "33", "44", "55", "66", "77"), List("55", "66", "7777"), List("1", "1", "1"))

  val toPad = s.map(_.length).max

  val paddedS = s.map(_.padTo(toPad, ""))

  val t = paddedS.transpose

  val llFunctor = Functor[List].compose(Functor[List])
  val l = llFunctor.map(t)(_.length).map(_.max)

  val separator = l.map("-" * _).mkString("+", "+", "+")

  def cell(v: String, width: Int): String = v.formatted(s"%${width}s")

  def row(v: List[String]): String = v.zip(l).map(Function.tupled(cell)).mkString("|", "|", "|")

  val rows = paddedS.map(row)

  val table = separator :: rows.head :: separator :: rows.tail ::: separator :: Nil

  println(table.mkString("\n"))
}