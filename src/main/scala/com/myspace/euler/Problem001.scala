package com.myspace.euler

object Problem001 extends App {
  val p1 = (1 until 1000).filter(i => (i % 3 == 0) || (i % 5) == 0).sum

  println(s"Problem1 = $p1")
}
