package com.myspace.euler

object Problem005 extends App {
  def primeFactors(n: Int): Seq[Int] = {
    def go(i: Int, d: Int, acc: Seq[Int]): Seq[Int] = {
      i match {
        case 1 => acc
        case s if s % d == 0 => go(s / d, d, acc :+ d)
        case s if s % d != 0 => go(s, d + 1, acc)
      }
    }

    go(n, 2, Seq[Int]())
  }

  def mergePrimeFactors(a1: Map[Int, Seq[Int]], a2: Map[Int, Seq[Int]]): Map[Int, Seq[Int]] = {
    def go(a1: Map[Int, Seq[Int]], a2: Map[Int, Seq[Int]], acc: Map[Int, Seq[Int]]): Map[Int, Seq[Int]] = {
      (a1, a2) match {
        case (m1, m2) if m1.isEmpty && m2.isEmpty => acc
        case (m, _) if m.nonEmpty => {
          val kv = m.head
          val p = acc get kv._1

          val newAcc = p.fold(acc + kv)(primes => if (kv._2.size > primes.size) acc + kv else acc)

          go(m.tail, a2, newAcc)
        }
        case (_, m) if m.nonEmpty => {
          val kv = m.head
          val p = acc get kv._1

          val newAcc = p.fold(acc + kv)(primes => if (kv._2.size > primes.size) acc + kv else acc)

          go(a1, a2.tail, newAcc)
        }
      }
    }

    go(a1, a2, Map[Int, Seq[Int]]())
  }

  val r = 2 to 20

  val seqPrimeFactors = r map primeFactors  // List(List(2), List(3), List(2,2), ...)
  val mapPrimeFactors = seqPrimeFactors map { l => l groupBy identity }
  val redPrimeFactors = mapPrimeFactors.fold(Map[Int, Seq[Int]]())((a1, a2) => mergePrimeFactors(a1, a2))
  val primes          = redPrimeFactors.values.flatten
  val result          = primes.product

//  println(s"pf: $seqPrimeFactors")
//  println(s"mf: $mapPrimeFactors")
//  println(s"rf: $redPrimeFactors")
  println(s"pp: $primes")

  println("Problem 5")
  println(s"result: $result")
}
