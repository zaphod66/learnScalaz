package com.myspace.learning

import scala.language.implicitConversions

import scalaz._, Scalaz._
import org.scalacheck.{Gen, Arbitrary}

import com.myspace.learning.day04._

class day04Test {

//implicit def COptionArb[A](implicit arb: Arbitrary[A]): Arbitrary[COption[A]] = arb map { a => CSome(0, a): COption[A] }
}
