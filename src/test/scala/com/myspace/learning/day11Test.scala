package com.myspace.learning

import com.myspace.learning.day11._

import scala.language.implicitConversions
import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Arbitrary

object day11Test extends App {
  implicit def turtleEqual: Equal[Turtle] = Equal.equalA

  implicit def TurtleArb(implicit arb: Arbitrary[Double]): Arbitrary[Turtle] = arb map { a =>
    Turtle(Point(a, a), a, Color(1.toByte, 1.toByte, 1.toByte))
  }

  val laws = lens.laws[Turtle, Double](turtleX)
  laws.check
}
