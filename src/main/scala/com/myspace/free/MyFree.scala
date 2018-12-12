package com.myspace.free

import scala.language.higherKinds

import scalaz.{-\/, \/, \/-}
import scalaz.Functor
import scalaz.syntax.functor._

object MyFree {

  case class Fix[F[_]](unfix: F[Fix[F]])

  case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

  case class Free[F[_]: Functor, A](resume: A \/ F[Free[F, A]]) {

    def pure(a: => A): Free[F, A] = Free(-\/(a))

    def map[B](f: A => B): Free[F, B] = this.resume match {
      case -\/(a) => Free(-\/(f(a)))
      case \/-(r) => Free(\/-(r.map(free => free.map(f))))
    }
    
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this.resume match {
      case -\/(a) => f(a)
      case \/-(r) =>
        val t = r.map(free => free.flatMap(f))
        Free(\/-(t))
    }
  }
}

object UseMyFree {
  import MyFree._

  sealed trait KVStoreA[A]
  case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  case class Get[T](key: String) extends KVStoreA[Option[T]]
  case class Del(key: String) extends KVStoreA[Unit]

  type KVStore[A] = Free[KVStoreA, A]

  
}