package com.myspace.trampoline

object FirstTrampoline {

  sealed trait Trampoline[+A] {
    final def run: A = this match {
      case Done(v) => v
      case More(k) => k().run
    }
  }

  case class Done[+A](result: A) extends Trampoline[A]
  case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

  case class StateSimple[S, +A](run: S => (A, S)) {
    def map[B](f: A => B) = StateSimple[S, B](s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

    def flatMap[B](f: A => StateSimple[S, B]) = StateSimple[S, B](s => {
      val (a, s1) = run(s)
      f(a) run s1
    })
  }
}

object TrampolineTest extends App {

  object testFirst {
    import FirstTrampoline._

    def even[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil => Done(true)
      case _ :: xs => More(() => odd(xs))
    }

    def odd[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil => Done(false)
      case _ :: xs => More(() => even(xs))
    }

    val l1 = List.fill(10000)('a')
    val b1e = even(l1).run
    val b1o = odd(l1).run

    val l2 = List.fill(10001)('a')
    val b2e = even(l2).run
    val b2o = odd(l2).run

    println(s"l1: ${l1.size}: even = $b1e, odd = $b1o")
    println(s"l2: ${l2.size}: even = $b2e, odd = $b2o")
  }
}
