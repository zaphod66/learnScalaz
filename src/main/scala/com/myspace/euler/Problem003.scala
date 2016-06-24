package com.myspace.euler

import scala.collection.immutable.Stream.cons

object Problem003 extends App {

  val n = 600851475143L
//val n = 13195L

  def fromL(start: Long, step: Long): Stream[Long] = cons(start, fromL(start+step, step))

  def isPrime(n: Long): Boolean = primes.takeWhile(p => p * p <= n).forall(n % _ != 0)
  val primes = 2L #:: cons(3L, fromL(3L + 2L, 2L)).filter(isPrime)

  def isPrimeFactor(i: Long): Boolean = {
    n % i == 0
  }

  val largestFactor = math.sqrt(n).toLong

  val factors = primes.takeWhile(_ < largestFactor).toList.reverse

  val r = factors.dropWhile(p => !isPrimeFactor(p))

  println(s"Problem3 = ${r.head}")

}
