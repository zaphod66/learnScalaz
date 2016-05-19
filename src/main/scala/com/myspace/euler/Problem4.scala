package com.myspace.euler

object Problem4 extends App {
  val range = 999 to 2 by -1

  def isPalindrome(n: Int): Boolean = {
    val s = n.toString

    s == s.reverse
  }

  val r1 = for {
    x <- range
    y <- range
    r = x * y if isPalindrome(r)
  } yield r

  println(s"Problem4 = ${r1.sorted.reverse.head}")
}
