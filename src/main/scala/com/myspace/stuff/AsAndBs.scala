package com.myspace.stuff

object SolutionNotStackSafe {
  import scalaz.StateT
  import scalaz.State

  case class ABState(as: Int, bs: Int, acc: String)

  val StateAB = StateT.stateMonad[ABState]
  import StateAB._

  val step: State[ABState, Unit] = State { s =>
    if (s.acc.isEmpty) {
      if (s.as >= s.bs)
        if (s.as > 1)
          (ABState(s.as - 2, s.bs, "aa"), ())
        else
          (ABState(s.as - 1, s.bs, "a"), ())
      else
        if (s.bs > 1)
          (ABState(s.as, s.bs - 2, "bb"), ())
        else
          (ABState(s.as, s.bs - 1, "b"), ())
    } else {
      if (s.acc.startsWith("a")) {
        if (s.bs >= s.as)
          (ABState(s.as, s.bs - 2, "bb" + s.acc), ())
        else
          (ABState(s.as, s.bs - 1, "b" + s.acc), ())
      } else {
        if (s.as >= s.bs)
          (ABState(s.as - 2, s.bs, "aa" + s.acc), ())
        else
          (ABState(s.as - 1, s.bs, "a" + s.acc), ())
      }
    }
  }

  val cond: State[ABState, Boolean] = for {
    s <- get
    flag = s.acc.isEmpty || s.acc.startsWith("b") && s.as > 0 || s.acc.startsWith("a") && s.bs > 0
//    _ = println(s"state = $s - cond = $flag")
  } yield flag

  val prog = whileM_(cond, step)

  def solution(as: Int, bs: Int): String = {
    val state = prog.exec(ABState(as, bs, ""))

    state.acc
  }
}

import scalaz.Scalaz._
import scalaz._
import scalaz.Free.Trampoline

object SolutionStackSafe {

  case class ABState(as: Int, bs: Int, acc: String)

  type ABStateM[A] = StateT[Trampoline, ABState, A]

  val stateT = StateT.stateTMonadState[ABState, Trampoline]
  import stateT._

  val step: ABStateM[Unit] = StateT { s =>
    if (s.acc.isEmpty) {
      if (s.as >= s.bs)
        if (s.as > 1)
          Monad[Trampoline].pure(ABState(s.as - 2, s.bs, "aa"), ())
        else
          Monad[Trampoline].pure(ABState(s.as - 1, s.bs, "a"), ())
      else
      if (s.bs > 1)
        Monad[Trampoline].pure(ABState(s.as, s.bs - 2, "bb"), ())
      else
        Monad[Trampoline].pure(ABState(s.as, s.bs - 1, "b"), ())
    } else {
      if (s.acc.startsWith("a")) {
        if (s.bs >= s.as)
          Monad[Trampoline].pure(ABState(s.as, s.bs - 2, "bb" + s.acc), ())
        else
          Monad[Trampoline].pure(ABState(s.as, s.bs - 1, "b" + s.acc), ())
      } else {
        if (s.as >= s.bs)
          Monad[Trampoline].pure(ABState(s.as - 2, s.bs, "aa" + s.acc), ())
        else
          Monad[Trampoline].pure(ABState(s.as - 1, s.bs, "a" + s.acc), ())
      }
    }
  }

  val cond: ABStateM[Boolean] = for {
    s <- get
    flag = s.acc.isEmpty || s.acc.startsWith("b") && s.as > 0 || s.acc.startsWith("a") && s.bs > 0
  } yield flag

  val prog = whileM(cond, step)(MonadPlus[List])

  def solution(as: Int, bs: Int): String = {
    val state = prog.run(ABState(as, bs, "")).run

    state._1.acc
  }
}

object AsAndBs extends App {

  def check(as: Int, bs: Int, f: (Int, Int) => String): Boolean = {
    val res = f(as, bs)

    val aa  = res.filter(_ == 'a').length
    val bb  = res.filter(_ == 'b').length

    println(s"""($as, $bs) => ("$aa", "$bb") - $res""")

    as == aa && bs == bb
  }

  println(check(10, 22, SolutionNotStackSafe.solution))
  println(check(1000, 20000, SolutionStackSafe.solution))

}
