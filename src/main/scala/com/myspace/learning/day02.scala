package com.myspace.learning

import scalaz._, Scalaz._

import scala.language.higherKinds

object day02 extends App {

  println("learning Scalaz day02")

  // Functions as Functors
  val f01 = Functor[List].lift{ (_: Int) * 2 }
  val r01 = f01(List(3))

  val f02 = List(1, 2, 3) >| "x"          // List(x, x, x)
  val f03 = List(1, 2, 3) as "x"          // List(x, x, x)
  val f04 = List(1, 2, 3).fpair           // List((1,1), (2,2), (3,3))
  val f05 = List(1, 2, 3).strengthL("x")  // List((x,1), (x,2), (x,3))
  val f06 = List(1, 2, 3).strengthR("x")  // List((1,x), (2,x), (3,x))
  val f07 = List(1, 2, 3).void            // List((), (), ())

  val f08 = List(1, 2, 3, 4) map { (_: Int) * (_: Int) }.curried
  val r08 = f08 map { _(9) }

  println(s"f08 map { _(9) } = $r08")

  // Applicative
  val l01 = 1.point[List]
  val l02 = 1.point[List] map { _ + 2 }
  val o01 = 1.point[Option]
  val o02 = 1.point[Option] map { _ + 2 }

  // Using Apply
  val o03 = 9.some <*> { (_: Int) + 3 }.some   // Some(12)
  val o04 = 1.some <* 2.some                   // Some(1)
  val o05 = none <* 2.some                     // None
  val o06 = 1.some *> 2.some                   // Some(2)
  val o07 = none *> 2.some                     // None

  // Option as Apply
  val o08 = 9.some <*> { (_: Int) + 3}.some    // Some(12)
  val o09 = 3.some <*> { 9.some <*> { (_: Int) + (_: Int) }.curried.some }  // Some(12)

  // Applicative Style
  val o10 = ^(3.some, 5.some) { _ + _ }       // Some(8)
  val o11 = ^(3.some, none[Int]) { _ + _ }    // None

  // Applicative Builder
  val o12 = (3.some |@| 5.some) { _ + _ }     // Some(8)

  // List as Apply (Lists (actually the list type constructor, []) are applicative Functors
  val l03 = List(1,2,3) <*> List((_: Int) * 2, (_: Int) + 100, (x: Int) => x * x)
  val l04 = List(3, 4) <*> { List(1, 2) <*> List( { (_: Int) + (_: Int) }.curried, { (_: Int) * (_: Int) }.curried ) }
  val l05 = ( List("ha", "heh", "hmm") |@| List("!", "?", ".") ) { _ + _ }

  // Useful functions for Applicative
  val a01 = Apply[Option].lift2( (_: Int) :: (_: List[Int]))
  val ra1 = a01(3.some, List(4).some)

  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil     => (Nil: List[A]).point[F]
    case x :: xs => (x |@| sequenceA(xs)) { _ :: _ }
  }

  val a02 = sequenceA(List(1.some, 2.some))
  val a03 = sequenceA(List(1.some, none, 2.some))
  val a04 = sequenceA(List(List(1, 2, 3), List(4, 5, 6)))

  type Function1Int[A] = ({type l[A] = Function1[Int, A]})#l[A]
  val a05 = sequenceA(List((_: Int) + 3, (_: Int) + 2, (_: Int) + 1): List[Function1Int[Int]])
  val ra5 = a05(2)

  println(s"ra5 = $ra5")
}
