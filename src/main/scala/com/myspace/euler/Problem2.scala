package com.myspace.euler

// Feven(n) = 4 ∗ Feven(n-1) + Feven(n−2)

object Problem2 extends App {
  val fibs: Stream[Int] = 0 #:: fibs.scanLeft(1)(_ + _)

  val l1 = fibs.takeWhile(_ < 4000000).toList
  val l2 = l1.filter(_ %2 == 0)
  val p2 = l2.sum

  println(s"Problem2 = $p2")
}
