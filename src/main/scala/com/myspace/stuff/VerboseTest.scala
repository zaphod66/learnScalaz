package com.myspace.stuff

object VerboseTrampoline {
  sealed trait Trampoline[+A] {
    final def run: A = {
      println(s"run ")
      this match {
        case Done(v) => { println(s"case Done($v) "); v }
        case More(k) => { print(s"<case More "); val t = k(); println("> "); t.run }
      }
    }
  }

  class Done[+A](val result: A) extends Trampoline[A]
  object Done {
    def apply[A](result: A): Done[A] = { println(s"Done.apply($result) "); new Done[A](result) }
    def unapply[A](arg: Done[A]): Option[A] = { println(s"Done.unapply "); Option(arg.result) }
  }

  class More[+A](val k: () => Trampoline[A]) extends Trampoline[A]
  object More {
    def apply[A](k: () => Trampoline[A]): More[A] = { println(s"More.apply "); new More[A](k) }
    def unapply[A](arg: More[A]): Option[() => Trampoline[A]] = { println(s"More.unapply "); Option(arg.k) }
  }
}

object VerboseTest extends App {
  import VerboseTrampoline._

  def even(n: Int): Trampoline[Boolean] = {
    println(s"even($n) ")
    if (n == 0) Done(true)
    else More(() => odd(n - 1))
  }

  def odd(n: Int): Trampoline[Boolean] = {
    println(s"odd($n) ")
    if (n == 0) Done(false)
    else More(() => even(n - 1))
  }

  println("=======")
  val e1 = even(20001)
  println("=======")
  val r1 = e1.run
  println("=======")

//  val o1 =  odd(4).run

  println(s"r1: $r1")
//  println(s"o1: $o1")
}

