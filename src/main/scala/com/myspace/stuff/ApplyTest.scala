package com.myspace.stuff

import scalaz._, Scalaz._

object ApplyTest extends App {

  val listApply = new Apply[List] {
    override def ap[A, B](fa: => List[A])(f: => List[(A) => B]): List[B] = {
      f flatMap { fp => fa map fp }
    }

    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }

  val l1 = List[Int](1, 2, 4, 8)
  val f1 = List[Int => Int](_ * 2, _ + 2)

  val r1 = listApply.ap(l1)(f1)
  val r2 = Apply[List].ap(l1)(f1)

  import scalaz.syntax.apply._

  val r3 = l1 <*> f1
  val r4 = l1 <*  f1
  val r5 = l1  *> f1

  println(s"$l1 => $r1")
  println(s"$l1 => $r2")
  println(s"$l1 => $r3")
  println(s"$l1 => $r4")
  println(s"$l1 => ${ r5.map( r => r(1) ) }")
}
