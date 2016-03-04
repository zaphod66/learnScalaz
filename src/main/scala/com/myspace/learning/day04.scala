package com.myspace.learning

import scalaz._, Scalaz._

import scala.language.implicitConversions

object day04 extends App {

  println("learning Scalaz day04")
  // Functor - for things that can be mapped over

  // Functor laws
  // Identity - mapping identity yields the input
  List(1, 2, 3) map identity assert_=== List(1, 2, 3)

  // Associativity
  (List(1, 2, 3) map {{(_: Int) * 3} map {(_: Int) + 1}}) assert_=== (List(1, 2, 3) map {(_: Int) * 3} map {(_: Int) + 1})

  // Option as Monoid
  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def zero: Option[A] = None

    def append(f1: Option[A], f2: => Option[A]) = (f1, f2) match {
      case (Some(a1), Some(a2)) => Some(Semigroup[A].append(a1, a2))
      case (Some(a1), None)     => f1
      case (None, Some(a2))     => f2
      case (None, None)         => None
    }
  }

  val o01 = (none: Option[String]) |+| "andy".some    // Some("andy")
  val o02 = (Ordering.LT: Ordering).some |+| none     // Some(LT)

  // mappend should take the first one if both are Some -> First tag
  val o03 = Tags.First('a'.some) |+| Tags.First('b'.some)           // Some('a')
  val o04 = Tags.First(none: Option[Char]) |+| Tags.First('b'.some) // Some('b')
  val o05 = Tags.First('a'.some) |+| Tags.First(none: Option[Char]) // Some('a')

  // mappend should take the second one if both are Some -> Last tag
  val o06 = Tags.Last('a'.some) |+| Tags.Last('b'.some)             // Some('b')
  val o07 = Tags.Last(none: Option[Char]) |+| Tags.Last('b'.some)   // Some('b')
  val o08 = Tags.Last('a'.some) |+| Tags.Last(none: Option[Char])   // Some('a')

  // Foldable - for things that can be folded up
  val f01 = List(1, 2, 3).foldRight (1) {_ * _}
  val f02 = 9.some.foldLeft(2) {_ + _}

  // for elements that are Monoids one can use foldMap to first map to a Monoid
  val f03 = List(1, 2, 3) foldMap identity
  val f04 = List(true, false, true, true) foldMap Tags.Disjunction.apply
}
