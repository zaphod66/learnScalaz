package com.myspace.learning

import scalaz._, Scalaz._

object day06 extends App {

  println("learning Scalaz day06")

  def isBigGang(x: Int): (Boolean, String) = (x > 9, "Compared gang size to 9.")

  implicit class PairOps[A, B: Monoid](pair: (A, B)) {
    def applyLog[C](f: A => (C, B)): (C, B) = {
      val (x, log1) = pair
      val (y, log2) = f(x)
      (y, log1 |+| log2)
    }
  }

  val w1 = (3, "smallish gang.") applyLog isBigGang

  val w2 = 3.set("smallish gang.")  // create a writer

  def logNumber(x: Int): Writer[List[String], Int] = x.set(List("Got number: " + x.shows))

  def multiWithLog: Writer[List[String], Int] = for {
    a <- logNumber(3)
    b <- logNumber(5)
  } yield a * b

  def gcdList(a: Int, b: Int): Writer[List[String], Int] = {
    if (b == 0)
      for {
        _ <- List("Finished with " + a.shows).tell
      } yield a
    else
      for {
        result <- gcdList(b, a % b)
        _ <- List(a.shows + " % " + b.shows + " = " + (a % b).shows).tell
      } yield result
//    List(a.shows + " % " + b.shows + " = " + (a % b).shows).tell.>>=( _ => gcdList(b, a % b) )
  }

  def gcdVect(a: Int, b: Int): Writer[Vector[String], Int] = {
    if (b == 0)
      for {
        _ <- Vector("Finished with " + a.shows).tell
      } yield a
    else
      for {
        result <- gcdVect(b, a % b)
        _ <- Vector(a.shows + " % " + b.shows + " = " + (a % b).shows).tell
      } yield result
  }

  def vectFinalCountDown(x: Int): Writer[Vector[String], Unit] = {

    @annotation.tailrec
    def doFinalCountDown(x: Int, w: Writer[Vector[String], Unit]): Writer[Vector[String], Unit] = x match {
      case 0 => w.flatMap( _ => Vector("0").tell )
      case x => doFinalCountDown(x - 1, w.flatMap( _ => Vector(x.shows).tell ) )
    }

    val t0 = System.currentTimeMillis
    val r = doFinalCountDown(x, Vector[String]().tell)
    val t1 = System.currentTimeMillis

    r.flatMap( _ => Vector((t1 - t0).shows + " msec").tell )
  }

  def listFinalCountDown(x: Int): Writer[List[String], Unit] = {

    @annotation.tailrec
    def doFinalCountDown(x: Int, w: Writer[List[String], Unit]): Writer[List[String], Unit] = x match {
      case 0 => w >>= { _ => List("0").tell }
      case x => doFinalCountDown(x - 1, w >>= { _ => List(x.shows).tell } )
    }

    val t0 = System.currentTimeMillis
    val r = doFinalCountDown(x, List[String]().tell)
    val t1 = System.currentTimeMillis

    r >>= { _ => List((t1 - t0).shows + " msec").tell }
  }

  val p1 = vectFinalCountDown(10000).run._1.last
  val p2 = listFinalCountDown(10000).run._1.last

  // functions are Functors
  val f0 = (_: Int) * 5
  val f1 = (_: Int) + 3
  val f2 = f1 map f0
  val r0 = f2(8)

  // functions are Applicative Functors
  val f3 = ( { (_: Int) * 2 } |@| { (_: Int) + 10 } ) { _ + _ }
  val r1 = f3(3)

  // functions are also Monads
  val f4: Int => Int = for {
    a <- (_: Int) * 2
    b <- (_: Int) + 10
  } yield a + b   // all functions read from a common source => functions are Reader Monads

  val r2 = f4(3)
}
