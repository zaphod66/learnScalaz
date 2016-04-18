package com.myspace.learning

import com.myspace.learning.day11._

import scala.language.implicitConversions
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Arbitrary

object day11Test extends App {
  implicit def turtleEqual: Equal[Turtle] = Equal.equalA

  implicit def PointArb(implicit arb: Arbitrary[(Double, Double)]): Arbitrary[Point] = arb map { case (x, y) =>
    Point(x, y)
  }

  implicit def ColorArb(implicit arb: Arbitrary[(Byte, Byte, Byte)]): Arbitrary[Color] = arb map { case (r, g, b) =>
    Color(r, g, b)
  }

  implicit def TurtleArb(implicit arb: Arbitrary[(Point, Double, Color)]): Arbitrary[Turtle] = arb map { case (p, h, c) =>
    Turtle(p, h, c)
  }

  val lawsTurtleX = lens.laws[Turtle, Double](turtleX)
  val lawsTurtleY = lens.laws[Turtle, Double](turtleY)
  val lawsHeading = lens.laws[Turtle, Double](turtleHeading)

  lawsTurtleX.check
  lawsTurtleY.check
  lawsHeading.check

}
