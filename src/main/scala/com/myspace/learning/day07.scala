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

  /////////////////////////////////////////////////////////////

  // \/

  val d1 = 1.right[String]
  val d2 = "boom".left[Int]

  val d3 = "boom".left[Int] >>= { x => (x + 1).right }
  val d4 = for {
    e1 <- "event 1 ok".right
    e2 <- "event 2 failed!".left[String]
    e3 <- "event 3 failed!".left[String]
  } yield e1 |+| e2 |+| e3

  val b1 = "ok".right.isRight // true
  val b2 = "bad".left.isRight  // false

  // getOrElse alias is '|'
  val d5 = "ok".right.getOrElse("something wrong")
  val d6 = "ok".left.getOrElse("something good")
  val d7 = "ok".right | "something wrong"
  val d8 = "bad".left | "something good"

  // map to transform the right side
  val d9 = "ok".right map { x => x + "!" }
  val d10 = "ok".left[String] map { x => x + "!"}

  // to chain on the left there is orElse, alias is '|||'
  val d11 = "ok".right.orElse("good".right) // \/-(ok)
  val d12 = "bad".left.orElse("good".right) // \/-(good)
  val d13 = "ok".right ||| "good".right     // \/-(ok)
  val d14 = "bad".left ||| "good".right     // \/-(good)

  /////////////////////////////////////////////////////////////

  // Validation (it's not a Monad, it's instead an Applicative Functor)

  val v1 = "event 1 ok".success[String]
  val v2 = "boom 1 fail".failure[String]

  val v3 = ("evnt 1 ok ".success[String] |@| "evnt 2 boom!".failure[String] |@| "evnt 3 boom!".failure[String]) { _ + _ + _ }

  // NonEmptyList

  val v4 = "evnt 1 ok!".successNel[String]
  val v5 = "evnt 2 boom!".failureNel[String]
  val v6 = ("evnt 1 ok".successNel[String] |@| "evnt 2 boom!".failureNel[String] |@| "evnt 3 boom!".failureNel[String]) {_ + _ + _}

  case class P(s: String, i: Int) {
    def +(that: P): P = P(this.s + " + " + that.s, this.i + that.i)
    override def toString: String = s + " = " + i
  }

  implicit val pSemiGroup = new Semigroup[P] {
    def append(f1: P, f2: => P): P = f1 + f2
  }

  val v7 = P("One", 1).success[String]
  val v8 = P("Two", 2).success[String]

  val v9 = (v7 |@| v8) { _ + _ }

  val v10 = "boom".failure[P]
  val v11 = (v7 |@| v10) { _ + _ }

  val v12 = v7 +++ v8
  val v13 = v7 +|+ v8
  val v14 = v7 +|+ v10
  val v15 = v10 +|+ v7
  val v16 = v10 +|+ v10

  val t = v9.getOrElse(P("Zero", 0))
}
