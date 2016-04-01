package com.myspace.whyfp90

object Whyfp90 extends App {

  // Newton-Raphson square roots
  def next(n: Double)(x: Double) = (x + (n / x)) / 2

  def squareRoot(n: Double) = next(n) _

  def repeat(f: Double => Double)(a: Double): Stream[Double] = a #:: repeat(f)(f(a))

  def s1(n: Double) = repeat(squareRoot(n))(n)
  val l1 = s1(2).take(10).toList

  println(l1)

  def within(eps: Double)(s: Stream[Double]): Double = s match {
    case a #:: b #:: ss if math.abs(a - b) <= eps => b
    case a #:: ss => within(eps)(ss)
  }

  val d1 = within(0.0001)(repeat(next(2) _)(2)) // Double = 1.4142135623746899

  def witSqrt(eps: Double)(n: Double) = within(eps)(repeat(next(n) _)(n))

  def wSqrt = witSqrt(0.0000001) _

  def relative(eps: Double)(s: Stream[Double]): Double = s match {
    case a #:: b #:: ss if math.abs((a / b) - 1) <= eps => b
    case a #:: ss => relative(eps)(ss)
  }

  def relSqrt(eps: Double)(n: Double) = relative(eps)(repeat(next(n) _)(n))

  def rSqrt = relSqrt(0.0000001) _

  println(s"sqrt(2) = ${ wSqrt(2) }, ${ rSqrt(2) }")
}
