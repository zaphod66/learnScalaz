package com.myspace.free

import scala.language.higherKinds

import scalaz.{-\/, \/, \/-}
import scalaz.Functor

object MyFree {

  case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

  case class Free[F[_]: Functor, A](resume: A \/ F[Free[F, A]]) {

    def pure[A](a: => A): Free[F, A] = Free(-\/(a))

    def map[B](f: A => B): Free[F, B] = this.resume match {
      case -\/(a) => Free(-\/(f(a)))
      case \/-(r) => Free(r.map(f))
    }
    
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = ???
  }
}
