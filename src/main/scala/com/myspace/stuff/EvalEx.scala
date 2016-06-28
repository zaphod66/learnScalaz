package com.myspace.stuff

object EvalEx extends App {

  val range = 1 until 10
  def f(d: Double)(n: Double): Double = n / d

  val functions = range map { i => f(i) _ }
  def combine(f1: Double => Double, f2: Double => Double) = (x: Double) => f1(x) * f2(x)
  val scanedFs = functions.scan((_ => 1.0): Double => Double)((f1, f2) => combine(f1, f2))

  def exp(x: Double) = (scanedFs map (f => f(x))).sum

  def f2 = f(2) _
  def f3 = f(3) _
  def c1 = combine(f2, f3)

  println(s"f2(3): ${f2(3)}")
  println(s"f3(3): ${f3(3)}")
  println(s"f2(3) * f3(3): ${f2(3) * f3(3)}")
  println(s"c1(3): ${c1(3)}")

  println(s"exp(20): ${exp(20)}")
}
