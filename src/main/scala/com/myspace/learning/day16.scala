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
}
