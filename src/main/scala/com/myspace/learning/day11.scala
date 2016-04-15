package com.myspace.learning

import scalaz._, Scalaz._

object day11 {
  case class Point(x: Double, y: Double)
  case class Color(r: Byte, g: Byte, b: Byte)
  case class Turtle(position: Point, heading: Double, color: Color)

  val t1 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

  val turtlePosition = Lens.lensu[Turtle, Point] ((a, value) => a.copy(position = value), _.position)
  val positionX = Lens.lensu[Point, Double]((a, value) => a.copy(x = value), _.x)
  val positionY = Lens.lensu[Point, Double]((a, value) => a.copy(y = value), _.y)

  val turtleX = turtlePosition >=> positionX  // symbolic alias for andThen
  val turtleY = turtlePosition >=> positionY
  val turtleHeading = Lens.lensu[Turtle, Double]((a, value) => a.copy(heading = value), _.heading)

  val t2 = turtleX.get(t1)
  val t3 = turtleX.set(t1, 5.0)
  val t4 = turtleX.mod(_ + 1.0, t1)
  val incX = turtleX =>= { _ + 1.0 }  // turtle => turtle
  val t5 = incX(t1)

//  We are now describing change of internal values upfront and passing in
//  the actual value at the end. Does this remind you of something?
//
//  Lens as a State Monad

  // %= method takes a function Double => Double and returns a State monad that expresses the change.

  val incX2 = for { x <- turtleX %= { _ + 1.0 } } yield x // State[Turtle, Double]
  val s1 = incX2(t1)  // (Turtle, Double)

  def forward(dist: Double) = for {
    h <- turtleHeading
    x <- turtleX += dist * math.cos(h)
    y <- turtleY += dist * math.sin(h)
  } yield (x, y)

  val t6 = forward(10.0)(t1)
  val t7 = forward(10.0) exec (t1)
}
