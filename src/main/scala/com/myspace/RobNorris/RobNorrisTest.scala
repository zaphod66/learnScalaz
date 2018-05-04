// https://www.youtube.com/watch?v=po3wmq4S15A
//
// An effect is whatever distinguishes F[A] from A.

package com.myspace.RobNorris

import scala.language.higherKinds

object RobNorrisTest extends App {

  object ReaderTest {
    case class Reader[E, A](run: E => A)

    type Config = String

    def f[A, B]: A => Reader[Config, B] = ???
    def g[B, C]: B => Reader[Config, C] = ???

    def h[A, C]: A => Reader[Config, C] = f andThen g

    // example
    type Host = String
    def path(s: String): Reader[Host, String] = Reader { host => s"http://$host/$s" }

    private val p = path("foo/bar")

    val p1 = p.run("google.com")
    val p2 = p.run("duckduck.go")

    println(s"p1: $p1")
    println(s"p2: $p2")
  }

  object WriterTest {
    case class Writer[W, A](w: W, a: A)

    type Info = List[String]

    def f[A, B]: A => Writer[Info, B] = ???
    def g[B, C]: B => Writer[Info, C] = ???

    def h[A, C]: A => Writer[Info, C] = f andThen g

    // example

    def toDouble(i: Int): Writer[Info, Double] = Writer(List(s"toDouble($i)"), i.toDouble)

    val t5 = toDouble(5)
    println(s"toDouble(5) = $t5")

  }

  object StateTest {
    case class State[S, A](run: S => (A, S))

    // example
    type Counter = Int

    def greet(name: String): State[Counter, String] = State {
      count => (s"Hello $name, you are person number $count", count + 1)
    }

    private val x = greet("Bob")

    val p1 = x.run(1)
    val p2 = x.run(19)
  }

  object Combinator {
    trait Fishy[F[_]] {
      def pure[A](a: A): F[A]
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

      //  def >==>[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]
    }

    implicit class FishyFunctionOps[F[_], A, B](f: A => F[B]) {
      def >==>[C](g: B => F[C])(implicit ev: Fishy[F]): A => F[C] =
        a => ev.flatMap(f(a))(g)
    }

    implicit val FishyOption: Fishy[Option] = new Fishy[Option] {
      override def pure[A](a: A): Option[A] = Some(a)
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    val char10: String => Option[Char] = s => s.lift(10)
    val letter: Char   => Option[Int]  = c => if (c.isLetter) Some(c.toInt) else None

    val hh: String => Option[Int] = char10 >==> letter
  }
}
