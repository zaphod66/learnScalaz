package com.myspace.stackless

import scala.language.implicitConversions

case class State[S, +A](runS: S => (A, S)) {

  def map[B](f: A => B) = State[S, B](s => {
    val (a, s1) = runS(s)
    val (b, s2) = (f(a), s1)

    println(s"map    : $s => ($b, $s2)")

    (b, s2)
  })

  def flatMap[B](f: A => State[S, B]) = State[S, B](s => {
    val (a, s1) = runS(s)
    val (b, s2) = f(a) runS s1

    println(s"flatMap: $s => ($b, $s2)")

    (b, s2)
  })
}

///////////

sealed trait Trampoline[+A] {
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
    case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
    case x => FlatMap(x, f)
  }

  def map[B](f: A => B): Trampoline[B] = flatMap(a => Done(f(a)))

  final def resume: Either[() => Trampoline[A], A] = this match {
    case Done(v)       => Right(v)
    case More(k)       => Left(k)
    case FlatMap(a, f) => a match {
      case Done(v)       => f(v).resume
      case More(k)       => Left(() => k() flatMap f)
      case FlatMap(b, g) => b.flatMap((x: Any) => g(x) flatMap f).resume
    }
  }

  final def runT: A = resume match {
    case Right(a) => a
    case Left(k)  => k().runT
  }

  final def runTsimple: A = this match {
    case More(k) => k().runTsimple
    case Done(v) => v
  }
}

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

case class Done[+A](v: A) extends Trampoline[A]

case class FlatMap[A, +B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]

///////////

object Stackless extends App {
  def getState[S]: State[S, S] = { /* print("getState->"); */ State(s => { println(s"$s => ($s, $s)"); (s, s)}) }

  def setState[S](s: S): State[S, Unit] = { /*print("setState->"); */ State(_ => { println(s"_ => (_, $s)"); ((), s)} ) }

  def pureState[S, A](a: A): State[S, A] = State(s => { println(s"$s => ($a, $s)"); (a, s)})

  def append[A](a: A, as: List[A]) = { println(s"$a::$as"); a::as }

  // produces a stackoverflow in flatMap on large lists
  def zipIndex[A](as: List[A]): List[(Int, A)] = as.foldLeft(
    pureState[Int, List[(Int, A)]](List())
  )((acc, a) => for {
    xs <- acc
    n  <- getState
    _  <- setState(n + 1)
  } yield append((n, a), xs)).runS(0)._1.reverse

  val l1 = List('a', 'b', 'c')
  println("------")
  val l2 = zipIndex(l1)
  println("------")

  println(s"l1: $l1")
  println(s"l2: $l2")

  println("======")

  // no tail-call elimination possible
  def even[A](ns: List[A]): Boolean = ns match {
    case Nil     => true
    case x :: xs => odd(xs)
  }

  def odd[A](ns: List[A]): Boolean = ns match {
    case Nil     => false
    case x :: xs => even(xs)
  }

  // Trampoline : Trading stack for heap

  def evenT[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil     => Done(true)
    case x :: xs => More( () => oddT(xs) )
  }

  def oddT[A](ns: List[A]): Trampoline[Boolean] = ns match {
    case Nil     => Done(false)
    case x :: xs => More( () => evenT(xs) )
  }

  val ra1 = 1 to 10000

  try {
    even(ra1.toList)
  } catch {
    case t: Throwable => println(s"catch:  $t")
  }

  val isEvenT = evenT(ra1.toList)
  val isEven1 = isEvenT.runTsimple
  val isEven2 = isEvenT.runT

  println(s"isEven: $isEven1, $isEven2")

  def fib(n: Int): Trampoline[Int] =
    if (n <= 1) Done(n) else for {
      f1 <- More( () => fib(n - 1))
      f2 <- More( () => fib(n - 2))
    } yield f1 + f2

  val fibT = fib(10)
  val fibn = fibT.runT

  println(s"fib(10) = $fibn")

  implicit def step[A](a: => A): Trampoline[A] = More( () => Done(a) )
}
