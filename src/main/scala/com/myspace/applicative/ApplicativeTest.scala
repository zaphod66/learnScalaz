package com.myspace.applicative

import scala.util.{Failure, Success, Try}
import scalaz.Validation

import scala.language.implicitConversions

object ApplicativeTest extends App {
  import scalaz.Applicative
  import scalaz.syntax.applicative._

  implicit val tryApplicative = new Applicative[Try] {
    def point[A](a: =>  A): Try[A] = Success(a)
    def ap[A, B](x: => scala.util.Try[A])(f: => scala.util.Try[A => B]) = f.flatMap(ff => x.map(ff))
  }

  implicit def successOrMessage[A](ma: Option[A], message: String): Try[A] = {
    ma match {
      case Some(a) => Success(a)
      case None    => Failure(new Throwable(message))
    }
  }

  val t1 = Try[Int](1)
  val t2 = Try[Int](2)
  val t3: Try[Int] = Failure(new RuntimeException("Foo"))

  val t4 = t1 tuple t2
  val t5 = t1 tuple t3

  val t6 = t1 |@| t2

  println(s"t1 tuple t2: $t4")
  println(s"t1 tuple t3: $t5")
  println(s"t1 + t2: ${t4 map (t => t._1 + t._2)}")
  println(s"t1 + t3: ${t5 map (t => t._1 + t._2)}")

  val o1: Option[Int] = Some(1)
  val o2: Option[Int] = None

  val t7 = successOrMessage(o1, "o1")
  val t8 = successOrMessage(o2, "o2")

  import scalaz.syntax.std.option._

  implicit def validationToTry[A](v: Validation[scala.Throwable,A]): Try[A] = {
    v match {
      case scalaz.Success(a) => scala.util.Success[A](a)
      case scalaz.Failure(e) => scala.util.Failure(e)
    }
  }

  val t10 = o1.toSuccess(new Throwable("o1"))
  val t11 = o2.toSuccess(new Throwable("o2"))

  val t12: Try[Int] = o1.toSuccess(new Throwable("o1"))
  val t13: Try[Int] = o2.toSuccess(new Throwable("o2"))

  println(s"t7 = $t7")
  println(s"t8 = $t8")
  println(s"t10 = $t10")
  println(s"t11 = $t11")
  println(s"t12 = $t12")
  println(s"t13 = $t13")
}
