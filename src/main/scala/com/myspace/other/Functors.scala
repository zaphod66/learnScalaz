package com.myspace.other

import scala.language.higherKinds

case class Pred[A](run: A => Boolean)

trait Func[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

trait ContraFunc[F[_]] {
  def contraMap[A, B](fa: F[A])(f: B => A): F[B]
}

object Functors extends App {

  // covariant functor
  val listFunc = new Func[List] {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }

  val l1 = List('a', 'b', 'c')
  val l2 = listFunc.fmap(l1)(c => c.toInt)

  println("l1: " + l1.mkString(" - "))
  println("l2: " + l2.mkString(" - "))

  // contra variant functor
  val predCoFunc = new ContraFunc[Pred] {
    override def contraMap[A, B](fa: Pred[A])(f: (B) => A): Pred[B] = Pred((b: B) => fa.run(f(b)))
  }

  val predI = Pred((i: Int) => i > 97)
  val predC = predCoFunc.contraMap(predI)((c: Char) => c.toInt)

  val l3 = l1 map predC.run

  println("l3: " + l3.mkString(" - "))
}
