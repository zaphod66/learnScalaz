package com.myspace.euler

object Problem006 extends App {
  val r = 1 to 100

  val sumOfSquares = r.map( x => x * x ).sum
  val squareOfSum  = r.sum * r.sum

  println(s"sumOfSquares = $sumOfSquares")
  println(s"squareOfSum  = $squareOfSum")


  println(s"Problem 6    = ${squareOfSum - sumOfSquares}")
}
