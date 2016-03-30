package com.myspace.learning

import scalaz._, Scalaz._

object day07 extends App {

  println("learning Scalaz day07")

  // Applicative Builder

  val a1 = (3.some |@| 5.some) { _ + _ }
  val a2 = ( { (_: Int) * 2 } |@| { (_: Int) + 10 } ) { _ + _ }
  val r1 = a2(2)

  type Stack = List[Int]

  def pop1(stack: Stack): (Int, Stack) = stack match { case x :: xs => (x, xs) }

  def push1(a: Int, stack: Stack): (Unit, Stack) = ((), a :: stack)

  def stackManip1(stack: Stack): (Int, Stack) = {
    val (_, newStack1) = push1(3, stack)
    val (a, newStack2) = pop1(newStack1)
    pop1(newStack2)
  }

  val s1 = stackManip1(List(5, 8, 2, 1))  // (5, List(8, 2, 1))

  // State Monad

  // construct a new State using the State Singleton
  val s0 = State[List[Int], Int] { case x :: xs => (xs, x) }

  val pop2  = State[Stack, Int] { case x :: xs => (xs, x) }

  def push2(a: Int) = State[Stack, Unit] { case xs => ( a :: xs, ()) }

  def stackManip2: State[Stack, Int] = for {
    _ <- push2(3)
    a <- pop2
    b <- pop2
  } yield b

  val s2 = stackManip2(List(5, 8, 2, 1))  // (List(8, 2, 1), 5)

  def stackyStack(l1: Stack, l2: Stack, l3: Stack): State[Stack, Unit] = for {
    stackNow <- get
    r <- if (stackNow === l1) put(l2) else put(l3)
  } yield r

  val stacker = stackyStack(List(1, 2, 3), List(2, 3, 4), List(3, 4, 5))
  val st1 = stacker(List(1, 2, 3)) //  (List(2, 3, 4),())
  val st2 = stacker(List(1, 2))    //  (List(3, 4, 5),())

  val pop3: State[Stack, Int] = for {
    s <- get[Stack]
    (x :: xs) = s
    _ <- put(xs)
  } yield x

  def push3(x: Int): State[Stack, Unit] = for {
    xs <- get[Stack]
    r <- put(x :: xs)
  } yield r

  def stackManip3: State[Stack, Int] = for {
    _ <- push3(3)
    a <- pop3
    b <- pop3
  } yield b

  val s3 = stackManip3(List(5, 8, 2, 1))  // (List(8, 2, 1), 5)
}
