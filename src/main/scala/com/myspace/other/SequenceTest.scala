package com.myspace.other

import scalaz._, Scalaz._

import scala.language.higherKinds

trait ApMonad[M[_]] extends Monad[M] {
  override def ap[A, B](fa: => M[A])(f: => M[A => B]): M[B] = super.ap(fa)(f)
  def app[A, B](ma: => M[A])(mf: => M[A => B]): M[B] = {
//    val l = for {
//      f: (A => B) <- mf
//      a: A <- ma
//    } yield f(a)

    lazy val ma0 = ma
    lazy val fab: (A => B) => M[B] = map(ma0)
    bind(mf)(fab)
  }
}

object SequenceTest extends App {
  println("SequenceTest")
}
