package com.myspace.logging

import scala.language.implicitConversions
import scala.language.reflectiveCalls

// see http://blog.tmorris.net/posts/the-writer-monad-using-scala-example/

trait Monoid[A] {
  def append(a1: A, a2: A): A
  def empty: A
}

object Monoid {
  implicit def ListMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def append(a1: List[A], a2: List[A]) = a1 ::: a2
    override def empty = Nil
  }
}

case class Logger[LOG, A](log: LOG, value: A) {
  def map[B](f: A => B) = Logger(log, f(value))
  def flatMap[B](f: A => Logger[LOG, B])(implicit m: Monoid[LOG]) = {
    val x = f(value)
    Logger(m.append(log, x.log), x.value)
  }
}

object Logger {
  def pure[LOG, A](value: A)(implicit m: Monoid[LOG]) = Logger(m.empty, value)
}

object Util {
  implicit def ListLogUtil[A](a: A) = new {
    def ~>[B](b: B) = Logger(List(a), b)
    def <|~[B](k: A => B) = Logger(List(k(a)), a)
  }

  def noLog[A](a: A) = Logger.pure[List[String], A](a)
}

object Writer extends App {
  import Util._

  def addOne(n: Int) = s"adding 1 to $n" ~> (n + 1)
  def intStr(n: Int) = s"toString of $n" ~> n.toString
  def lenEven(s: String) = s"checking $s.length for evenness" ~> (s.length % 2 == 0)
  def hundredOrThousand(b: Boolean) = Logger(List(s"checked $b -> 100|1000"), if (b) 100 else 1000)
  def minOne(n: Int) = n - 1
  def times7(n: Int) = (n * 7) <|~ (r => "multiplying " + n + " by 7 yields " + r)

  val x = 5

  val r = for {
    a <- addOne(x)
    b <- intStr(a)
    c <- lenEven(b)
    d <- hundredOrThousand(c)
    g <- noLog(minOne(d))
    e <- times7(g)
  } yield e

  println(s"Result: ${r.value}")
  println
  println("LOG")
  println("===")
  r.log foreach println
}
