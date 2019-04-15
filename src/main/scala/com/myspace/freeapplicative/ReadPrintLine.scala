package com.myspace.freeapplicative

import scalaz.{Coyoneda, Free}

sealed trait Console[A]
case class ReadLine[A](value: String => A) extends Console[A]
case class PrintLine[A](line: String, value: A) extends Console[A]
/*
object ReadPrintLineExample extends App {
  println("First Read-/PrintLine")

//  type DSL[A] = Coyoneda[Console, A]

  def readLine: Free[Console, String] = Free.liftF(ReadLine(identity))
  def printLine(line: String): Free[Console, String] = Free.liftF(PrintLine(line, ""))

  val program = for {
    line <- readLine
    _    <- printLine("You wrote: " + line)
  } yield ()
}
*/