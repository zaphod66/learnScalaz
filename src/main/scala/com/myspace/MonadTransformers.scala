package com.myspace

import scalaz._, Scalaz._

object MonadTransformers extends App {

  // to model: 1. Result, 2. No Result, 3. Error
  type Result[+A] = String \/ Option[A]

  val r1: Result[Int] = some(42).right[String]

  // to use r1 we have to unwrap it twice
  val t1 = for {
    o <- r1
  } yield {
    for {
      value <- o
    } yield value.toString
  } // scalaz.\/[String, Option[String]]

  val t2 = r1 map { _ map { _.toString } }

  // Monad transformers to the rescue

  type Error[A] = \/[String, A]
  type Res[A] = OptionT[Error, A]

  val r2 = 42.point[Res]
  val t3 = for {
    value <- r2
  } yield value.toString

  // see http://underscore.io/blog/posts/2013/12/20/scalaz-monad-transformers.html

  val r3 = r2 map { _ + 1 }
  val r4 = r3 flatMap { _ => "Yeah!".point[Res]}

  // all monad transformers return their data on run
  println(s"r2 = $r2, r2.run = ${r2.run}")
  println(s"t3 = $t3, t3.run = ${t3.run}")
  println(s"r3 = $r3, r3.run = ${r3.run}")
  println(s"r4 = $r4, r4.run = ${r4.run}")

  val r5 = r4.run.fold(err => "Boom!", ok => "Yeah!")

  // on OptionT:
  // flatMap[B](f: A => OptionT[F, B]): OptionT[F, B]
  // flatMapF[B](f: A => F[B]: OptionT[F, B]

  def positive(in: Int): \/[String, Boolean] = if (in > 0) true.right else "negative".left

  val r6 = r2 flatMapF positive
  val r7 = -2.point[Res] flatMapF positive

  println(s"r5 = $r5")
  println(s"r6 = $r6, r6.run = ${r6.run}")
  println(s"r7 = $r7, r7.run = ${r7.run}")

}
