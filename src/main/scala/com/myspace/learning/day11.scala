package com.myspace.learning

import scalaz._, Scalaz._

object day11 {
  case class Point(x: Double, y: Double)
  case class Color(r: Byte, g: Byte, b: Byte)
  case class Turtle(position: Point, heading: Double, color: Color)

  val t1 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))

  val turtlePosition = Lens.lensu[Turtle, Point] ((a, value) => a.copy(position = value), _.position)
  val positionX = Lens.lensu[Point, Double] ((a, value) => a.copy(x = value), _.x)
  val positionY = Lens.lensu[Point, Double] ((a, value) => a.copy(y = value), _.y)

  val turtleX = turtlePosition >=> positionX
  val turtleY = turtlePosition >=> positionY
}
