package com.myspace.learning

import scala.language.implicitConversions

import scalaz._, Scalaz._

import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Arbitrary

object day04Test extends App {
  // Functor Laws
  println("==========\nFunctor List")
  functor.laws[List].check

  // Breaking the functor law
  sealed trait COption[+A] {}
  case class CSome[A](counter: Int, a: A) extends COption[A]
  case object CNone extends COption[Nothing]

  implicit def coptionEqual[A]: Equal[COption[A]] = Equal.equalA
  implicit val coptionFunctor = new Functor[COption] {
    def map[A, B](fa: COption[A])(f: A => B): COption[B] = fa match { case CNone => CNone
    case CSome(c, a) => CSome(c + 1, f(a))
    }
  }

  implicit def COptionArb[A](implicit arb: Arbitrary[A]): Arbitrary[COption[A]] = arb map { a => CSome(0, a): COption[A] }
  println("==========\nFunctor COption")
  functor.laws[COption].check
}
