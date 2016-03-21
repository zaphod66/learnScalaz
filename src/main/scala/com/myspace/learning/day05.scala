package com.myspace.learning

import scalaz._, Scalaz._

object day05 extends App {

  println("learning Scalaz day05")

  val o1 = 3.some flatMap { x => (x + 1).some }
  val o2 = Monad[Option].point("WHAT")
  val o3 = Monad[Option].point(9) flatMap { x => Monad[Option].point(x * 10) }

  // Walk the line

  type Birds = Int
  case class Pole(left: Birds, right: Birds) {
    def landLeft(n: Birds): Option[Pole] =
      if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none

    def landRight(n: Birds): Option[Pole] =
      if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
      else none

    def banana: Option[Pole] = none
  }

  val p1 = Pole(0, 0).landLeft(2)
  val p2 = Pole(0, 3).landLeft(10)
  val p3 = Pole(0, 0).landRight(1).flatMap(_.landLeft(2))
  val p4 = Monad[Option].point(Pole(0, 0)) flatMap { _.landRight(2) } flatMap { _.landLeft(4) }
  val p5 = Monad[Option].point(Pole(0, 0)) >>= { _.landRight(2) } >>= { _.landLeft(2) } >>= { _.landRight(2) }
  val o4 = none >> 3.some // None
  val o5 = 3.some >> 4.some // 4.some
  val o6 = 3.some >> (none: Option[Int]) // none
  // operator precedence breaks the type inference (starts or ends with '=', make low precedence) you have to use parens
  val p6 = Monad[Option].point(Pole(0, 0)).>>=({_.landLeft(2)}).>>(none: Option[Pole]).>>=({_.landRight(2)})

  val f1 = 3.some >>= { x => "!".some >>= { y => (x.shows + y).some } } // Some("3!")
  val f2 = for {
      x <- 3.some
      y <- "!".some
    } yield x.shows + y

  val f3 = for {
    p0 <- Monad[Option].point(Pole(0, 0))
    p1 <- p0.landLeft(2)
    p2 <- p1.landRight(2)
    p3 <- p2.landLeft(1)
  } yield p3

  def justH(str: String): Option[Char] = for {
    (x :: xs) <- str.toList.some
  } yield x

  // List Monad
  val l1 = ^(List(1, 2, 3), List(10, 20, 30)){ _ * _ }
  val l2 = ^(List(1, 2, 3), List(10, 20, 30)){ (_, _) }
  val l3 = (List(1, 2, 3) |@| List(10, 20, 30)) { (_, _) }  // l2 === l3, for performance consider using ^ (Apply)

  val l4 = List(1, 2, 3) >>= { x => List(-x, x) }
  val l5 = for { n <- List(1, 2); c <- List('a', 'b') } yield (n, c)

  // MonadPlus introduces 'filter'
  val l6 = (1 |-> 50) filter { x => x.shows contains '7' }
  val l7 = for { x <- 1 |-> 50 if x.shows contains '7' } yield x

  val l8 = List(1, 2, 3) <+> List(4, 5, 6)  // List[Int] = List(1, 2, 3, 4, 5, 6)

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
