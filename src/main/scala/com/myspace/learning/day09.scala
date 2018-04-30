package com.myspace.learning

import scalaz._, Scalaz._

object day09 extends App {

  println("learning Scalaz day09")

  // Trees

  def freeTree: Tree[Char] =
    'P'.node(
      'O'.node(
        'L'.node('N'.leaf, 'T'.leaf),
        'Y'.node('S'.leaf, 'A'.leaf)
      ),
      'L'.node(
        'W'.node('C'.leaf, 'R'.leaf),
        'A'.node('A'.leaf, 'C'.leaf)
      )
    )

  def changeToP(tree: Tree[Char]): Tree[Char] = tree match {
    case Tree.Node(x, Stream(
           l, Tree.Node(y, Stream(
             Tree.Node(_, Stream(m, n)), r)))) => x.node(l, y.node('P'.node(m, n), r))
  }

  val f1 = freeTree.loc.getChild(2) >>= { _.getChild(1) } >>= { _.modifyLabel({_ => 'P'}).some }
  val t1 = f1 map { _.toTree }

  t1 foreach { _.drawTree foreach { _.println } }

  // Zipper

  val s1 = Stream(1, 2, 3, 4)
  val z1 = s1.toZipper

  val z2 = z1 >>= { _.next }
  val z3 = z1 >>= { _.next } >>= { _.next }

  val z4 = z3 >>= { _.modify { _ + 1 }.some }

  val o1 = z4 map { _.toStream.toList }
  val l1 = o1.sequenceU.flatten   // List(1, 2, 4, 4)

  println(s"l1 = $l1")

  val z5 = for {
    z <- s1.toZipper
    n1 <- z.next
    n2 <- n1.next
  } yield n2.modify { _ + 1}

  val l2 = (z5 map { _.toStream.toList }).sequenceU.flatten

  // Id
  // type Id[+X] = X

  val i1 = 0: Id[Int]
  val i2 = 1 visit { case x@(2|3) => List(x * 2) }  // List(1)
  val i3 = 2 visit { case x@(2|3) => List(x * 2) }  // List(4)

  def f[A](tree: Tree[A]): Unit = tree match {
    case Tree.Node(n, s) => { println(n); s foreach f }
    case _               => println("--")
  }
}
