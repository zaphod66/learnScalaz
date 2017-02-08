package com.myspace.other

object HackerRankTest extends App {
  private def dist(p1: (Int, Int), p2: (Int, Int)): Double = {
    val x1 = p1._1
    val y1 = p1._2
    val x2 = p2._1
    val y2 = p2._2

    0.0D
  }

  val numPoints = scala.io.StdIn.readInt

  val inputs = scala.collection.mutable.ListBuffer[(Int, Int)]()

  for (pair <- 1 to numPoints) {
    val line = scala.io.StdIn.readLine
    val splits = line.split(" ")

    val x = splits(0).toInt
    val y = splits(1).toInt

    inputs += (x -> y)
  }

  val points = inputs.toList
  val rotate = points.tail ++ List(points.head)
  val pairs  = points zip rotate

  val perimeter = pairs.foldLeft(0.0D)((acc, pair) => acc + dist(pair._1, pair._2))

  println(s"points: $points, $rotate, $pairs")
  println(s"perimeter: $perimeter")
}
