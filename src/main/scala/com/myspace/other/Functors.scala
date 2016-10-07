package com.myspace.other

import scala.language.higherKinds
import scala.language.postfixOps

import scalaz._
import Kleisli._

case class Pred[A](run: A => Boolean)
case class Comp[A](run: (A, A) => Int)// (implicit ordering: Ordering[A])
case class Arro[A, B](run: A => B)

trait Func[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

trait ContraFunc[F[_]] {
  def contraMap[A, B](fa: F[A])(f: B => A): F[B]
}

trait ProFunc[F[_, _]] {
  def dimap[A, B, C, D](fbc: F[B, C])(f: A => B, g: C => D): F[A, D]
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
    override def contraMap[A, B](fa: Pred[A])(f: (B) => A): Pred[B] = Pred(b => fa.run(f(b)))

    def contraMap2[A, B](fa: Pred[A])(f: (B) => A): Pred[B] = fa match {
      case Pred(run) => Pred(run compose f)
    }
  }

  val predI = Pred((i: Int) => i > 97)
  val predC = predCoFunc.contraMap(predI)((c: Char) => c.toInt)

  val l3 = l1 map predC.run

  println("l3: " + l3.mkString(" - "))

  val compCoFunc = new ContraFunc[Comp] {
    override def contraMap[A, B](fa: Comp[A])(f: (B) => A): Comp[B] = Comp((b1, b2) => fa.run(f(b1), f(b2)))
  }

  val compI = Comp((x: Int, y: Int) => x compare y)
  val compC = compCoFunc.contraMap(compI)((c: Char) => c.toInt)

  val b4 = compC.run('a', 'b')
  val l4 = l1.sortWith(compC.run(_, _) > 0)

  println("l4: " + l4.mkString(" - "))

  // pro functor

  val arroProFunc = new ProFunc[Arro] {
    override def dimap[A, B, C, D](fbc: Arro[B, C])(f: A => B, g: C => D): Arro[A, D] =
      Arro(g compose fbc.run compose f)
  }

  def fItoD(i: Int): Double = i * 1.5
  val arroID = Arro(fItoD)
  val arroCS = arroProFunc.dimap(arroID)((c: Char) => c.toInt, (d: Double) => d.toString)

  val l5 = l1 map arroCS.run

  println("l5: " + l5.mkString(" - "))

  def fItoMD = (i: Int) => Option(fItoD(i))

  type OKleisli[A, B] = Kleisli[Option, A, B]

  val kleisliProFunc = new ProFunc[OKleisli] {
    override def dimap[A, B, C, D](fbc: OKleisli[B, C])(f: (A) => B, g: (C) => D): OKleisli[A, D] = {
      val ac = fbc.run compose f
      def ad(a: A) = ac(a).flatMap(c => Option(g(c)))

      Kleisli(ad)
    }
  }

  val funky1 = kleisli(fItoMD)
  val funky2 = kleisliProFunc.dimap(funky1)((c: Char) => c.toInt, (d: Double) => d.toString)

  val l6 = l1 map funky2
  println("l6: " + l6.mkString(" - "))

}
