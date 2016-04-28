package com.myspace.learning

import scalaz._, Scalaz._

object day16 extends App {

  // Memo

  def slowFib: Int => Int = {
    case 0 => 0
    case 1 => 1
    case n => slowFib(n - 2) + slowFib(n - 1)
  }

  val f1 = logTime { slowFib(30) }
  val f2 = logTime { slowFib(40) }
  val f3 = logTime { slowFib(45) }

  val memoFib: Int => Int = Memo.mutableHashMapMemo {
    case 0 => 0
    case 1 => 1
    case n => memoFib(n - 2) + memoFib(n - 1)
  }

  val f4 = logTime { memoFib(30) }
  val f5 = logTime { memoFib(40) }
  val f6 = logTime { memoFib(45) }

  def logTime[T](e: => T): T = {
    val start = System.currentTimeMillis()
    val r = e
    val stop  = System.currentTimeMillis()

    println(s"took ${stop - start} msec")

    r
  }

  // Effect System

  import effect._
  import ST.{newVar, runST, newArr, returnST}

  def e1[S](i: Int) = for {
    x <- newVar[S](i)
    r <- x mod { _ + 1 }
  } yield x

  def e2[S] = for {
    x <- e1[S](0)
    r <- x.read
  } yield r

  type ForallST[A] = Forall[({type l[S] = ST[S, A]})#l]

  val i1 = runST(new ForallST[Int] { def apply[S] = e2[S] })

  // Sieve of Eratosthenes

  def mapM[A, S, B](xs: List[A])(f: A => ST[S, B]): ST[S, List[B]] =
    Monad[({type l[A] = ST[S, A]})#l].sequence(xs map f)

  def sieve[S](n: Int) = for {
    arr <- newArr[S, Boolean](n + 1, true)
    _ <- arr.write(0, false)
    _ <- arr.write(1, false)

    nsq = math.sqrt(n.toDouble).toInt + 0

    _ <- mapM (1 |-> nsq) { i =>
      for {
        x <- arr.read(i)
        _ <- {
          if (x) mapM (i * i |--> (i, n)) { j => arr.write(j, false) }
          else returnST[S, List[Boolean]] { Nil }
        }
      } yield ()
    }

    r <- arr.freeze
  } yield r

  def primes(n: Int) =
    runST(new ForallST[ImmutableArray[Boolean]] { def apply[S] = sieve[S](n) }).toArray.zipWithIndex.
      collect { case (true, x) => x}

  val p1 = primes(100)

  p1 foreach { i => print(s"$i ") }
  println
}
