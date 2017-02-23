package com.myspace.hackerrank

object BigGCD extends App {
  println("BigGCD")

  def primeFactors(n: Int): Seq[Int] = {
    def go(m: Int, divisor: Int, factors: Seq[Int]): Seq[Int] = {
      if (m == 1) {
        factors
      } else {
        if (m % divisor == 0) {
          go(m / divisor, divisor, divisor +: factors)
        } else {
          go(m, divisor + 1, factors)
        }
      }
    }

    go(n, 2, Seq.empty[Int])
  }

  def primeFactorsList(ns: Seq[Int]): Seq[Int] = {
    ns flatMap primeFactors
  }

//  val l1 = Seq[Int](2, 2, 3, 3, 25)
//  val l2 = Seq[Int](8, 1, 6, 170)
  val l1 = List.fill(2)(8)
  val l2 = List.fill(2)(6)

  val f1 = primeFactorsList(l1)
  val f2 = primeFactorsList(l2)

  val f3 = f1 diff f2
  val r1 = f1 diff f3

  val s1 = (r1 map { n => BigInt(n) }).product

  println("==============")
//  l1 foreach { n => println(s"$n -> ${primeFactors(n)}")}
//  println("--------------")
  println(s"p($l1) = $f1")
  println(s"p($l2) = $f2")
  println(s"d(   ) = $f3")
  println(s"r(   ) = $r1")
  println(s"s(   ) = $s1")

}
