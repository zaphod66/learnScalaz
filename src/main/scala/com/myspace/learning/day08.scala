package com.myspace.learning

import scalaz._, Scalaz._

import scala.language.implicitConversions

object day08 extends App {

  println("learning Scalaz day08 - Monadic functions")

  val m1 = Option(1.some).join
  val m2 = Option(none).join
  val m3 = List(List(1,2,3), List(1,2,3)).join
  val m4 = 9.right[String].right[String].join
  val m5 = "boom".left[Int].right[String].join

  // filterM, foldLeftM, ...

  val f1 = List(1, 2, 3) filterM { x => List(true, false) }
  val f2 = Vector(1, 2, 3) filterM { x => Vector(true, false) }

  def binSmalls(acc: Int, x: Int): Option[Int] = {
    if (x > 8) none: Option[Int] else (acc + x).some
  }

  val f3 = List(2, 8, 3, 1).foldLeftM(0) { binSmalls }  // Some(14)
  val f4 = List(2, 9, 3, 1).foldLeftM(0) { binSmalls }  // None

  // Safe RPN

  def folding1(l: List[Double], next: String): List[Double] = (l, next) match {
    case (x :: y :: ys, "*") => (y * x) :: ys
    case (x :: y :: ys, "+") => (y + x) :: ys
    case (x :: y :: ys, "-") => (y - x) :: ys
    case (xs, numStr)        => numStr.toInt :: xs
  }

  def evalRPN1(s: String): Double =
    s.split(' ').toList.foldLeft(Nil: List[Double]) { folding1 }.head

  val d1 = evalRPN1("10 4 3 + 2 * -")

  val o1 = "1".parseInt.toOption  // Some(1)
  val o2 = "a".parseInt.toOption  // None

  def folding2(l: List[Double], next: String): Option[List[Double]] = (l, next) match {
    case (x :: y :: ys, "*") => ((y * x) :: ys).point[Option]
    case (x :: y :: ys, "+") => ((y + x) :: ys).point[Option]
    case (x :: y :: ys, "-") => ((y - x) :: ys).point[Option]
    case (xs, numStr)        => numStr.parseInt.toOption map { _ :: xs }
  }

  def evalRPN2(s: String): Option[Double] = for {
    List(x) <- s.split(' ').toList.foldLeftM(Nil: List[Double]) { folding2 }
  } yield x

  def folding3(l: List[Double], next: String): List[Double] = (l, next) match {
    case (x :: y :: ys, "*") => ((y * x) :: ys).point[List].flatten
    case (x :: y :: ys, "+") => ((y + x) :: ys).point[List].flatten
    case (x :: y :: ys, "-") => ((y - x) :: ys).point[List].flatten
    case (xs, numStr)        => numStr.parseInt.toList.flatMap(x => x.toDouble :: xs)
  }

//  def evalRPN3(s: String) = for {
//    List(x) <- s.split(' ').toList.foldLeftM(Nil: List[Double]) { folding3 }
//  } yield x

  // Kleisli : a wrapper for a function A => M[B]

  val k1 = Kleisli { (x: Int) => (x + 1).some }
  val k2 = Kleisli { (x: Int) => (x * 2).some }

  val d2 = 4.some >>= (k1 <=< k2) // (k1 compose k2)  Some(9)
  val d3 = 4.some >>= (k1 >=> k2) // (k1 andThen k2)  Some(10)

  // Reader again
  // type ReaderT[F[+_], E, A] = Kleisli[F, E, A]

  val addStuff: Reader[Int, Int] = for {
    a <- Reader { (_: Int) * 2 }
    b <- Reader { (_: Int) + 2 }
  } yield a + b

  // Making Monads

  case class Prob[A](list: List[(A, Double)])

  trait ProbInstances {
    implicit def probShow[A]: Show[Prob[A]] = Show.showA

    implicit val probInstance = new Functor[Prob] with Monad[Prob] {
      override def map[A, B](fa: Prob[A])(f: A => B): Prob[B] = Prob(fa.list map { case (x, p) => (f(x), p) })

      def point[A](a: => A): Prob[A] = Prob((a, 1.0) :: Nil)
      def bind[A, B](fa: Prob[A])(f: A => Prob[B]): Prob[B] = flatten(map(fa)(f))
    }

    def flatten[B](xs: Prob[Prob[B]]): Prob[B] = {
      def multAll(innerXs: Prob[B], p: Double) = innerXs.list map { case (x, r) => (x, p * r) }

      Prob(xs.list flatMap { case (innerXs, p) => multAll(innerXs, p)})
    }
  }

  case object Prob extends ProbInstances

  val p1 = Prob((3, 0.5) :: (5, 0.25) :: (9, 0.25) :: Nil)
  val p2 = p1 map { -_ }

  // Coin example

  sealed trait Coin

  case object Heads extends Coin
  case object Tails extends Coin

  implicit val coinEquals: Equal[Coin] = Equal.equalA

  def balancedCoin: Prob[Coin] = Prob(Heads -> 0.5 :: Tails -> 0.5 :: Nil)
  def loadedCoin:   Prob[Coin] = Prob(Heads -> 0.1 :: Tails -> 0.9 :: Nil)

  def flipThree: Prob[Boolean] = for {
    a <- balancedCoin
    b <- balancedCoin
    c <- loadedCoin
  } yield { List(a, b, c) all { _ === Tails }}

  val p3 = flipThree

  println(s"p3 = $p3")
}
