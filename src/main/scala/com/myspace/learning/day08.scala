package com.myspace.learning

import scalaz._, Scalaz._

object day08 extends App {

  println("learning Scalaz day08 - Monadic functions")

  val m1 = Option(1.some).join
  val m2 = Option(none).join
  val m3 = List(List(1,2,3), List(1,2,3)).join
  val m4 = 9.right[String].right[String].join
  val m5 = "boom".left[Int].right[String].join

  // filterM, foldLeftM, ...

  val f1 = List(1, 2, 3) filterM { x => List(true, false) }
  val f2 = Vector(1, 2, 3) filterM { x => Vector(true, false) }

  def binSmalls(acc: Int, x: Int): Option[Int] = {
    if (x > 8) none: Option[Int] else (acc + x).some
  }

  val f3 = List(2, 8, 3, 1).foldLeftM(0) { binSmalls }  // Some(14)
  val f4 = List(2, 9, 3, 1).foldLeftM(0) { binSmalls }  // None

  // Safe RPN

  def folding1(l: List[Double], next: String): List[Double] = (l, next) match {
    case (x :: y :: ys, "*") => (y * x) :: ys
    case (x :: y :: ys, "+") => (y + x) :: ys
    case (x :: y :: ys, "-") => (y - x) :: ys
    case (xs, numStr)        => numStr.toInt :: xs
  }

  def evalRPN1(s: String): Double =
    s.split(' ').toList.foldLeft(Nil: List[Double]) { folding1 }.head

  val d1 = evalRPN1("10 4 3 + 2 * -")

  val o1 = "1".parseInt.toOption  // Some(1)
  val o2 = "a".parseInt.toOption  // None

  def folding2(l: List[Double], next: String): Option[List[Double]] = (l, next) match {
    case (x :: y :: ys, "*") => ((y * x) :: ys).point[Option]
    case (x :: y :: ys, "+") => ((y + x) :: ys).point[Option]
    case (x :: y :: ys, "-") => ((y - x) :: ys).point[Option]
    case (xs, numStr)        => numStr.parseInt.toOption map { _ :: xs }
  }

  def evalRPN2(s: String): Option[Double] = for {
    List(x) <- s.split(' ').toList.foldLeftM(Nil: List[Double]) { folding2 }
  } yield x
}
