package com.myspace.IoMonad

import scalaz._

sealed trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }

  def ++(io: IO[Unit]): IO[Unit] = new IO[Unit] {
    def run = { self.run; io.run }
  }

}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)

  def point[A](a: => A): IO[A] = unit(a)
  def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  def empty: IO[Unit] = new IO[Unit] { def run = () }
}

object IoTest extends App {
  object FirstStep {
    def printFrame(msg: String) = {
      def printLine(s: String): IO[Unit] = new IO[Unit] { def run = println(s) }
      def printDeco(s: String): IO[Unit] = new IO[Unit] { def run = println(s"|$s|") }

      val line = List.fill(msg.length)('-').mkString("+", "", "+")
      printLine(line) ++ printDeco(msg) ++ printLine(line)
    }

    val c1 = printFrame("Plus")
    val c2 = printFrame("Quamperfekt")

    c1.run
    c2.run
  }

  object Converter {
    def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0
    def ReadLine: IO[String] = IO { scala.io.StdIn.readLine() }
    def PrintLine(s: String): IO[Unit]  = IO { println(s) }

    def converter: IO[Unit] = for {
      _ <- PrintLine("Enter temp in Fahrenheit: ")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(s"$d Fahrenheit = ${fahrenheitToCelsius(d)} Celsius.")
    } yield ()

    converter.run
  }

  object Factorial {

  }
  FirstStep

  Converter
}
