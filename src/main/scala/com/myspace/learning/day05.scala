package com.myspace.learning

import scalaz._, Scalaz._

object day05 extends App {

  println("learning Scalaz day05")

  // MonadPlus introduces 'filter'
  val l1 = (1 |-> 50) filter { x => x.shows contains '7' }

  // A knight's quest
  case class KnightPos(c: Int, r: Int) {
    def move: List[KnightPos] =
      for {
        KnightPos(c2, r2) <- List(
          KnightPos(c + 2, r - 1), KnightPos(c + 2, r + 1),
          KnightPos(c - 2, r - 1), KnightPos(c - 2, r + 1),
          KnightPos(c + 1, r - 2), KnightPos(c + 1, r + 2),
          KnightPos(c - 1, r - 2), KnightPos(c - 1, r + 2))
        if ((1 |-> 8) contains c2) && ((1 |-> 8) contains r2)
      } yield KnightPos(c2, r2)

    def in3: List[KnightPos] =
      for {
        fst <- move
        snd <- fst.move
        trd <- snd.move
      } yield trd

    def canReachIn3(end: KnightPos): Boolean = in3 contains end
  }


}
