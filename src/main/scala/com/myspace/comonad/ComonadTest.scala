package com.myspace.comonad

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A,B](x: F[A])(f: A => B): F[B]
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: A): M[A]
  def join[A](mma: M[M[A]]): M[A]
}

case class Reader[R, A](run: R => A) {
  def ask: Reader[R, R] = Reader(r => r)
}

object ComonadTest extends App {
  println("Comonad Test")

  type Config = Map[String, Int]

  val env = Map("value1" -> 1, "value2" -> 2)

  def ask[R]: Reader[R, R] = Reader(r => r)

  val reader = Reader((config: Config) => config.get(_) )

  val v = reader.run(env)("value1")
  val a = reader.ask.run(env)

  println(s"v = $v")
}
