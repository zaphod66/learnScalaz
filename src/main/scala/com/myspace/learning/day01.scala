package com.myspace.learning

import scalaz._, Scalaz._
import scala.language.implicitConversions

trait CanTruthy[A] { self =>
  /** @return true, if `a` is truthy. */
  def truthys(a: A): Boolean
}

object CanTruthy {
  def apply[A](implicit ev: CanTruthy[A]): CanTruthy[A] = ev

  def truthys[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
    override def truthys(a: A): Boolean = f(a)
  }
}

trait CanTruthyOps[A] {
  def self: A
  implicit def F: CanTruthy[A]

  final def truthy: Boolean = F.truthys(self)
}

object toCanIsTruthysOps {
  implicit def toCanIsTruthyOps[A](v: A)(implicit ev: CanTruthy[A]) =
    new CanTruthyOps[A] {
      def self = v
      implicit def F: CanTruthy[A] = ev
    }
}

object day01 extends App {

  import toCanIsTruthysOps._

  println("learning Scalaz day01")

  val t1 = 1.0 ?|? 2.0

  println(s"t1 = $t1")

  val cmin = implicitly[Enum[Char]].min
  val cmax = implicitly[Enum[Char]].max

  val imin = implicitly[Enum[Int]].min
  val imax = implicitly[Enum[Int]].max

  println(s"cmin: $cmin")
  println(s"cmax: $cmax")

  println(s"imin: $imin")
  println(s"imax: $imax")

  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.truthys({
    case 0 => false
    case _ => true
  })

  implicit def listCanTruthy[A]: CanTruthy[List[A]] = CanTruthy.truthys({
    case Nil => false
    case _   => true
  })

  println(s"10.truthy = ${10.truthy}")
  println(s"""Nil.truthy = ${List[String]().truthy}""")
  println(s"""List("foo").truthy = ${List("foo").truthy}""")

  def truthlyIf[A: CanTruthy, B, C](cond: A)(ifYes: => B)(ifNo: => C) =
    if (cond.truthy) ifYes else ifNo

  val ts = truthlyIf (List[String]()) ("Yeah") ("No")
  val ti = truthlyIf (3) ("Yeah") ("No")

  println(s"""truthlyIf (List[String]()) ("Yeah") ("No") = ${ts}""")
  println(s"""truthlyIf (3) ("Yeah") ("No") = ${ti}""")
}
