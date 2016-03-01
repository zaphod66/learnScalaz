package com.myspace.Bruch

import scala.language.implicitConversions

case class Bruch(zaehler: Int, nenner: Int) {
  override def toString(): String = {
    s"($zaehler / $nenner)"
  }

  def add(anderer: Bruch): Bruch = {
    val neuerN = nenner * anderer.nenner

    val neuerZ1 = zaehler * anderer.nenner
    val neuerZ2 = anderer.zaehler * nenner

    val neuerZ = neuerZ1 + neuerZ2

    val teiler = ggt(neuerZ, neuerN)

    Bruch(neuerZ / teiler, neuerN / teiler)
  }

  def sub(that: Bruch): Bruch = {
    add(Bruch(-that.zaehler, that.nenner))
  }

  def mul(anderer: Bruch): Bruch = {
    val neuerZ = zaehler * anderer.zaehler
    val neuerN = nenner  * anderer.nenner

    val teiler = ggt(neuerZ, neuerN)
    Bruch(neuerZ / teiler, neuerN / teiler)
  }

  def div(that: Bruch): Bruch = mul(that.kehr)

  def +(that: Bruch) = add(that)
  def -(that: Bruch) = sub(that)
  def *(that: Bruch) = mul(that)
  def /(that: Bruch) = div(that)

  def ggt(a: Int, b: Int): Int = {
    if (a < 0) ggt(-a,b) else
      if (b < 0) ggt(a, -b) else
        if (b > a) ggt(b, a) else
          if (a == b) a
          else ggt(a - b, b)
  }

  def kehr: Bruch = Bruch(nenner, zaehler)
}

object Bruch {
  def apply(z: Int): Bruch = Bruch(z, 1)
  implicit def toBruch(i: Int): Bruch = Bruch(i)
}

object BruchTest extends App {
  import Bruch._

  println("Hallo Nils!")
  println("===========")

  val a = Bruch( 9, 14)
  val b = Bruch(21, 27)

  println(s"$a + $b = ${a + b}")
  println(s"$a - $b = ${a - b}")
  println(s"$a * $b = ${a * b}")
  println(s"$a / $b = ${a / b}")
  println(s"2 * $a = ${2 * a}")
}
