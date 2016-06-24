package com.myspace.euler

object Problem047 extends App {
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

  def ff(ai: Array[(Seq[Int], Int)]): Option[Int] = {
    def go(i: Int): Option[Int] = {
      if (i > ai.length - 5) None
      else if ((ai(i + 0)._2 == ai(i + 1)._2 - 1) && (ai(i + 1)._2 == ai(i + 2)._2 - 1) && (ai(i + 2)._2 == ai(i + 3)._2 - 1)) Some(i)
      else go(i + 1)
    }

    go(0)
  }

  val r = 1 to 135000
  println("primeFactors")
  val m = r map { i => primeFactors(i) }
  println("indexing")
  val z = m.zipWithIndex map { case (p, i) => (p, i + 1) }
  println("distinction")
  val fz = z map { case (p, i) => (p.toSet, i) } map { case (p, i) => (p.toSeq, i) }
  println("filtering")
  val f = fz filter { case (p, i)  => p.size == 4 }
  println("arraying")
  val a = f.toArray
  println("finding")
  val idx = ff(a)

  println("Problem 47")

  idx foreach { i => (i to i + 3) foreach { k => println(s"n = ${a(k)._2} - ${primeFactors(a(k)._2)}") } }
}
