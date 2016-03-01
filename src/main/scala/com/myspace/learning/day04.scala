package com.myspace.learning

import scalaz._, Scalaz._

object day04 extends App {

  println("learning Scalaz day04")

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
}
