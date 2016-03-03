package com.myspace.learning

import scala.language.higherKinds
import scalaz._, Scalaz._

object day03 extends App {

  println("learning Scalaz day03")

//  scala> :k Int
//  scala.Int's kind is A
//
//  scala> :k -v Int
//  scala.Int's kind is A
//  *
//  This is a proper type.
//
//  scala> :k -v Option
//  scala.Option's kind is F[+A]
//  * -(+)-> *
//  This is a type constructor: a 1st-order-kinded type.
//
//  scala> :k -v Either
//  scala.util.Either's kind is F[+A1,+A2]
//  * -(+)-> * -(+)-> *
//  This is a type constructor: a 1st-order-kinded type.
//
//  scala> :k -v Equal
//  scalaz.Equal's kind is F[A]
//  * -> *
//  This is a type constructor: a 1st-order-kinded type.
//
//  scala> :k -v Functor
//  scalaz.Functor's kind is X[F[A]]
//  (* -> *) -> *
//  This is a type constructor that takes type constructor(s): a higher-kinded type.

  val f01 = Functor[List].lift((_: Int) + 2)
  val r01 = f01(List(1,2,3))

  // Tagged Types

  sealed trait KiloGram
  def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)
  val mass = KiloGram(20.0)

  val r02  = 2 * Tag.unwrap(mass)
  val r03  = 2 * scalaz.Tag.unsubst[Double, Id, KiloGram](mass)

  sealed trait JoulesPerKiloGram
  def JoulesPerKiloGram[A](a: A): A @@ JoulesPerKiloGram = Tag[A, JoulesPerKiloGram](a)

  def energyRel(m: Double @@ KiloGram): Double @@ JoulesPerKiloGram = {
    val c = 299792458.0

    JoulesPerKiloGram(c * c * Tag.unsubst[Double, Id, KiloGram](m))
  }

  val r04  = energyRel(mass)
  val r05  = energyRel(KiloGram(10))
  List(1, 2, 3) mappend List(4, 5, 6)

  // Monoids

  val m01 = List(1, 2, 3) |+| List(4, 5, 6)
  val m02 = "one" |+| "two"
  val m03 = Monoid[List[Int]].zero
  val m04 = Monoid[Option[Int]].zero
  val m05 = Monoid[String].zero
  val m06 = Monoid[Int].zero
  val m07 = Monoid[Int @@ Tags.Multiplication].zero

  println(s"m03 = $m03, m04 = $m04, m05 = $m05, m06 = $m06, m07 = $m07")

  val m08 = Tags.Multiplication(10) |+| Monoid[Int @@ Tags.Multiplication].zero

  val m09 = Tags.Disjunction(true) |+| Tags.Disjunction(false)
  val m10 = Monoid[Boolean @@ Tags.Disjunction].zero |+| Tags.Disjunction(true)
  val m11 = Monoid[Boolean @@ Tags.Disjunction].zero |+| Monoid[Boolean @@ Tags.Disjunction].zero
  val m12 = Monoid[Boolean @@ Tags.Conjunction].zero |+| Tags.Conjunction(true)
  val m13 = Monoid[Boolean @@ Tags.Conjunction].zero |+| Tags.Conjunction(false)

  // Ordering as Monoid

  val m14 = (Ordering.LT: Ordering) |+| (Ordering.GT: Ordering)   // scalaz.Ordering = LT
  val m15 = (Ordering.GT: Ordering) |+| (Ordering.LT: Ordering)   // scalaz.Ordering = GT
  val m16 = Monoid[Ordering].zero |+| (Ordering.LT: Ordering)     // scalaz.Ordering = LT
  val m17 = Monoid[Ordering].zero |+| (Ordering.GT: Ordering)     // scalaz.Ordering = GT
  val m18 = Monoid[Ordering].zero                                 // scalaz.Ordering = EQ

  // Because the left comparison is kept unless it’s Ordering.EQ we can use this to compose two levels of comparisons.
  def lengthCompare(lhs: String, rhs: String): Ordering = (lhs.length ?|? rhs.length) |+| (lhs ?|? rhs)

  val m19 = lengthCompare("zen", "ants")    // scalaz.Ordering = LT
  val m20 = lengthCompare("zen", "ant")     // scalaz.Ordering = GT
}
