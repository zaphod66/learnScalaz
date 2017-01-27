package com.myspace.stateM

import scalaz._, Scalaz._

object StateMonadTest extends App {
  sealed abstract class Tree[A] {

    def number(seed: Int): (Tree[(A, Int)], Int) = this match {
      case Leaf(a) => (Leaf(a, seed), seed + 1)
      case Branch(left, right) => left number seed match {
        case (l, ls) => right number ls match {
          case (r, rs) => (Branch(l ,r), rs)
        }
      }
    }

    def numberSM: State[Int, Tree[(A, Int)]] = this match {
      case Leaf(a) => for {
          s <- init[Int]
          _ <- modify((_: Int) + 1)
        } yield Leaf((a, s))
      case Branch(left, right) => for {
          l <- left.numberSM
          r <- right.numberSM
        } yield Branch(l ,r)
    }

    def numberSA: State[Int, Tree[(A, Int)]] = this match {
      case Leaf(a) => (init[Int] <* modify((_: Int) + 1)) map { s: Int => Leaf((a, s)) }
      case Branch(left, right) => for {
        l <- left.numberSA
        r <- right.numberSA
      } yield Branch(l, r)
    }
  }

  final case class Leaf[A](a: A) extends Tree[A]
  final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  val tree1 = Branch(Leaf("one"), Branch(Leaf("two"), Leaf("three")))
  val treel1 = tree1.number(1)
  val treel2 = tree1.numberSM(1)
  val treel3 = tree1.numberSA(1)

  println(s"Tree1:  $tree1")
  println(s"Treel1: $treel1")
  println(s"Treel2: $treel2")
  println(s"Treel3: $treel3")
}
