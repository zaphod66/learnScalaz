package com.myspace.eratosthenes

import org.scalameter._

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

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  val END = 20000
  var primesToTake = 0
  var primes1: List[Int] = Nil
  var primes2: List[Int] = Nil
  var primes3: List[Int] = Nil

  val time2 = standardConfig measure { primes2 = calcPrimeStream(END).toList }
  val time3 = standardConfig measure { primes3 = primeIter(END); primesToTake = primes3.size }
  val time1 = standardConfig measure { primes1 = primeStream().take(primesToTake).toList }

  println(s"primes1: ${primes1.size} ($time1 ms)")
  println(s"primes2: ${primes2.size} ($time2 ms)")
  println(s"primes3: ${primes3.size} ($time3 ms)")
}
