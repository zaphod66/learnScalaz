package com.myspace.other

import scalaz._
import Scalaz._

object TransformerTest extends App {
  println("TransformerTest")

  type Response[A] = String \/ Option[A]

  val cnt00: Response[Int] = some(10).right
  val cnt01: Response[Int] = None.right

  val res00 = for {
    mayBeCount <- cnt00
  } yield {
    for {
      c <- mayBeCount
    } yield c + 1
  }

  val res01 = for {
    c1M <- res00
    c2M <- cnt00
  } yield {
    for {
      c1 <- c1M
      c2 <- c2M
    } yield c1 + c2
  }

  val res02 = for {
    c1M <- res00
    c2M <- cnt01
  } yield {
    for {
      c1 <- c1M
      c2 <- c2M
    } yield c1 + c2
  }

  println(s"cnt00 $cnt00")
  println(s"res00 $res00")
  println(s"res01 $res01")
  println(s"res02 $res02")

  type Error[A]  = String \/ A
  type Result[A] = OptionT[Error, A]

  import scalaz.syntax.monad._

  val cnt10: Result[Int] = 10.point[Result]
  val cnt11: Result[Int] = OptionT.none

  val res10 = for {
    c <- cnt10
  } yield c + 1

  val res11 = for {
    c1 <- cnt10
    c2 <- res10
  } yield c1 + c2

  val res12 = for {
    c1 <- cnt10
    c2 <- cnt11
  } yield c1 + c2

  println(s"cnt10 $cnt10")
  println(s"res10 $res10")
  println(s"res11 $res11")
  println(s"res11 $res12")
  println(s"res10 ${res10.run}")
  println(s"res11 ${res11.run}")
  println(s"res11 ${res12.run}")
}
