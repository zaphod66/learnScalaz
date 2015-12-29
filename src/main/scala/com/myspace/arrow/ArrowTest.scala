package com.myspace.arrow

import scalaz._
import Scalaz._

import scala.language.higherKinds
/*
trait MyCategory[->[-_, +_]] {
  def id[A]: A -> A
  def compose[A, B, C](f: B -> C, g: A -> B): A -> C
}

trait MyArrow[->[-_, +_]] extends MyCategory {
  def arr[B, C](f: B => C): B -> C

  def fst[B, C, D](f: B -> C): (B, D) -> (C, D)
  def snd[A, B, C](f: A -> B): (C, A) -> (C, B)

  def &&&[B, C1, C2](fbc1: B -> C1, fbc2: B -> C2): B -> (C1, C2)
  def ***[B1, C1, B2, C2](fbc1: B1 -> C2, fbc2: B2 -> C2): (B1, B2) -> (C1, C2)
}

trait MyArrowChoice[->[-_, +_]] extends MyArrow {
  type E[A, B] = Either[A, B]

  def left[B, C, D](a: B -> C): E[B, D] -> E[C, D]
  def right[B, C, D](a: B -> C): E[D, B] -> E[D, C]

  def +++[B1, C1, B2, C2](a: B1 -> C1, b: B2 -> C2): E[B1, B2] -> E[C1, C2]
  def |||[B, C, D](a: B -> D, b: C -> D): E[B, C] -> D
}
*/
object ArrowTest extends App {

  val f: String => Int    = str => str.length
  val g: String => String = str => str.toUpperCase
  val h: Int    => String = n   => "-" * n

  val r = f &&& g
  val s = f *** g

  println(s"""f &&& g: r('Hello')          = ${r("Hello")}""")
  println(s"""f *** g: s('Hello', 'World') = ${s("Hello", "World")}""")

}
