package com.myspace.euler

object Problem7 extends App {
  def primeStream(stream: Stream[Int] = Stream.from(3, 2)): Stream[Int] =
    stream.head #:: primeStream(stream.tail.filter(_ % stream.head != 0))

  def primesIter(end: Int): List[Int] = {
    import scala.collection.mutable

    val primeIndices = mutable.ArrayBuffer.fill((end + 1) / 2)(1)

    val intSqrt = Math.sqrt(end).toInt
    for (i <- 3 to end by 2 if i <= intSqrt) {
      for (nonPrime <- i * i to end by 2 * i) {
        primeIndices.update(nonPrime / 2, 0)
      }
    }

    val seq = for {
      i <- primeIndices.indices if primeIndices(i) == 1
    } yield 2 * i + 1

    seq.tail.toList
  }

  // one less, because out primes start at 3 for efficiency
  val prime1 = primeStream().take(10000).toList.reverse.head
  val prime2 = primesIter(104743).reverse.head

  println(s"primeStream().take(10001).toList.reverse.head: $prime1")
  println(s"primes(104743): $prime2")
}
