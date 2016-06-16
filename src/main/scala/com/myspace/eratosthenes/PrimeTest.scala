package com.myspace.eratosthenes

object PrimeTest extends App {

  def primeStream(stream: Stream[Int] = Stream.from(3, 2)): Stream[Int] =
    stream.head #:: primeStream(stream.tail.filter(_ % stream.head != 0))

  def calcPrimeStream(end: Int): Stream[Int] = {
    val alls = Stream.from(3, 2).takeWhile(_ <= end)
    val odds = Stream.from(3, 2).takeWhile(_ <= Math.sqrt(end).toInt)
    def muls(i: Int) = Stream.from(i * i, 2 * i).takeWhile(_ <= end)

    def comp = odds.flatMap(i => muls(i))

    alls.diff(comp)
  }

  def primeIter(end: Int): List[Int] = {
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

  val primes1 = primeStream().take(29).toList
  val primes2 = calcPrimeStream(120).toList
  val primes3 = primeIter(120)

  println(s"primes1: $primes1")
  println(s"primes2: $primes2")
  println(s"primes3: $primes3")
}
