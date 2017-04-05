package com.myspace.io

import scalaz.effect.{ IO => SIO }
import scalaz._, Scalaz._

object ScalazIoTest extends App {
  val io1 = SIO {
    println("Hello IO!")
  }

  val io2 = SIO {
    println("Bye IO!")
  }

  val io3 = io1.unsafeZip(io2)

  def readLine: SIO[String] = SIO.readLn
  def printLine(s: String): SIO[Unit] = SIO.putStrLn(s)
  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  val io4 = for {
    _ <- printLine("Enter temp in fahrentheit:")
    s <- readLine
    d = s.toDouble
    _ <- printLine(s"$d Fahrenheit = ${fahrenheitToCelsius(d)} Celsius.")
  } yield ()

  println("=========")
  io1.unsafePerformIO()
  println("=========")
  io3.unsafePerformIO()
  println("=========")
  io4.unsafePerformIO()
  println("=========")
}
