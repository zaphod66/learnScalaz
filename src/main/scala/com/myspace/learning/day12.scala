package com.myspace.learning

import scalaz._, Scalaz._

import scala.language.higherKinds
import scala.language.reflectiveCalls

object day12 {

  // Origami programming

  // DList
  val dl1 = DList.unfoldr(10, { (x: Int) => if (x == 0) none else (x, x - 1).some } )
  val l1  = dl1.toList

  // unfold for Stream
  val s1 = unfold(10) { (x) => if (x == 0) none else (x, x - 1).some }
  val l2 = s1.toList

  def minimumS[A: Order](stream: Stream[A]): A = stream match {
    case x #:: xs => xs.foldLeft(x) { _ min _ }
  }

  def deleteS[A: Equal](y: A, stream: Stream[A]): Stream[A] = (y, stream) match {
    case (_, Stream()) => Stream()
    case (y, x #:: xs) => if (y === x) xs else x #:: deleteS(y, xs)
  }

  def delMin[A: Order](stream: Stream[A]): Option[(A, Stream[A])] = stream match {
    case Stream() => none
    case xs =>
      val y = minimumS(xs)
      (y, deleteS(y, xs)).some
  }

  def selectionSort[A: Order](stream: Stream[A]): Stream[A] = unfold(stream) { delMin[A] }

  // The Essence of the Iterator Pattern

  // Monoidal Applicatives

  // turn a Monoid to an Applicative

  val a1 = Monoid[Int].applicative
  val i1 = a1.ap2(1, 2)(0)
  val l3 = Monoid[List[Int]].applicative.ap2(List(1), List(2))(Nil)

  // Applicatives are closed under products
  val a2 = Applicative[List].product[Option]

  val p1 = a2.point(1)
  val p2 = a2.point(2)

  val p3 = (p1 |@| p2) { _ |+| _ }

  val p4 = (List(1), 1.success[String])
  val p5 = (List(2), "boom".failure[Int])

  val p6 = (p4 |@| p5) { _ |+| _ }

  // unlike monads, applicatives are also closed under composition

  val a3 = Applicative[List].compose[Option]

  val p7 = a3.point(1)

  // idiomatic traversal

  val l4 = List(1, 2, 3) traverse { x => (x > 0) option (x + 1) }
  val l5 = List(2, 3, 0) traverse { x => (x > 0) option (x + 1) }

  // for monadic applicatives traversal specialises to monadic map
  // similar to flatMap but the pass function yields G[B] where [G : Applicative] instead of List

  // for monoidal applicatives, traversal accumulates values
  val i2 = Monoid[Int].applicative.traverse(List(1,2,3)) { _ + 1 }

  def contents[F[_]: Traverse, A](f: F[A]): List[A] = Monoid[List[A]].applicative.traverse(f) { List(_) }
  def contents2[F[_]: Traverse, A](f: F[A]): List[A] = f.traverse[({type l[X]=List[A]})#l, A] { List(_) }

  val tree: Tree[Char] = 'P'.node('O'.leaf, 'L'.leaf)

  val l6 = contents(tree)

  def shape[F[_]: Traverse, A](f: F[A]): F[Unit] = f.traverse( _ => (): Id[Unit] )

  def decompose[F[_]: Traverse, A](f: F[A]) = (shape(f), contents(f))

  val d1 = decompose(tree)  // traverses the tree twice

  def decompose2[F[_]: Traverse, A](f: F[A]) =
    Applicative[Id].product[({type l[X]=List[A]})#l].traverse(f) { x => ((): Id[Unit], List(x)) }

  // Sequence

  val s2 = List(1.some, 2.some).sequence  // => Some(List(1, 2))
  val s3 = List(1.some, none).sequence    // => None

  val t1 = 1.success[String].node(2.success[String].leaf, 3.success[String].leaf)
  val t2 = t1.sequence[({type l[X]=Validation[String, X]})#l, Int]

  val t3 = 1.success[String].node(2.success[String].leaf, "boom".failure[Int].leaf)
  val t4 = t3.sequence[({type l[X]=Validation[String, X]})#l, Int]

  // Two kind of traversals:
  // First: accumulate elements effectfully a -> m() and an independent pure transformation a -> b

  // Traverse add traverseS, which is a specialized version of traverse for State

  def collect[F[_]: Traverse, A, S, B](t: F[A])(f: A => B)(g: S => S) =
    t.traverseS[S, B] { a => State { (s: S) => (g(s), f(a)) } }

  val loop = collect(List(1, 2, 3, 4))( _ * 2 )( (_: Int) + 11 )
  val p0 = loop.run(0)
  val p1 = loop.run(5)

  // Second: pure transformation but dependent on the state a -> b -> c,
  // evolving the state with a computation of m b

  def label[F[_]: Traverse, A](f: F[A]): F[(A,Int)] =
    f.traverseS { a =>
      for {
        n <- get[Int]
        x <- put(n + 1)
      } yield (a, n)
    } eval 0

  val l6 = label(List(10, 2, 8))
  val l7 = label(tree)
}
