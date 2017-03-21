package com.myspace.stuff

import scala.concurrent._
import scala.language.higherKinds
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object StackedMonads extends App {

  def getCount: Future[Option[Int]] = Future { Option(1) }

  // nested map
  val v1 = getCount.map(o => o.map(_ + 1))
  val v2 = getCount

  // nested for
  val s1 = for {
    fst <- v1
    snd <- v2
  } yield {
    for {
      r1 <- fst
      r2 <- snd
    } yield {
      r1 + r2
    }
  }

  // or, desugared
  val s2 = v1.flatMap(o1 =>
    v2.map(o2 =>
      o1.flatMap(r1 =>
        o2.map(r2 =>
          r1 + r2
        )
      )
    )
  )

  // Creating an Option Transformer

  trait Monad[T[_]] {
    def map[A, B](v: T[A])(f: A => B): T[B]
    def flatMap[A, B](v: T[A])(f: A => T[B]): T[B]
    def pure[A](x: A): T[A]
  }

  implicit val futureMonad = new Monad[Future] {
    def map[A, B](v: Future[A])(f: A => B) = v.map(f)
    def flatMap[A, B](v: Future[A])(f: A => Future[B]) = v.flatMap(f)
    def pure[A](x: A): Future[A] = Future(x)
  }

  case class OptionTransformer[T[_], A](v: T[Option[A]])(implicit monad: Monad[T]) {
    def map[B](f: A => B): OptionTransformer[T, B] = {
      OptionTransformer[T, B](monad.map(v)(a => a.map(f)))
    }
    def flatMap[B](f: A => OptionTransformer[T, B]): OptionTransformer[T, B] = {
      val res = monad.flatMap(v){
          case None    => monad.pure(None: Option[B])
          case Some(a) => f(a).v
        }

      OptionTransformer[T, B](res)
    }
  }

  //////////

  val t1 = OptionTransformer(v1)
  val t2 = OptionTransformer(v2)

  val s3 = for {
    fst <- t1
    snd <- t2
  } yield {
    fst + snd
  }

  val i1 = Await.result(s1,   Duration.Inf)
  val i2 = Await.result(s2,   Duration.Inf)
  val i3 = Await.result(s3.v, Duration.Inf)

  println(s"i1 = $i1, i2 = $i2, i3 = $i3")

  //////////

  import scalaz._, Scalaz._

  val fo = OptionT[List, Int](List(Some(1), Some(2), None, Some(4)))

  val ll = for {
    k <- fo
  } yield k + 1

  ll.run foreach println
}
