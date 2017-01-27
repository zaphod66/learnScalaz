package com.myspace.trampoline

import scala.util.{Try, Failure}

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

  def getState[S]: StateSimple[S, S]           = StateSimple(s => (s, s))
  def setState[S](s: S): StateSimple[S, Unit]  = StateSimple(_ => ((), s))
  def pureState[S, A](a: A): StateSimple[S, A] = StateSimple(s => (a, s))

  def zipIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(
      pureState[Int, List[(Int, A)]](List.empty[(Int,A)])
    )((acc, a) => for {
      xs <- acc
      n  <- getState
      _  <- setState(n + 1)
    } yield (n,a)::xs).run(0)._1.reverse
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

    val z3 = try { zipIndex(List.fill(10)('a')) } catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z3: $z3")

    val z4 = try { zipIndex(List.fill(10000)('b')) } catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z4: $z4")
  }

  testFirst
}
