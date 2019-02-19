package com.myspace.recursionscheme

import matryoshka.{Algebra, Coalgebra}
import scalaz.{Functor, INil, NonEmptyList}

// https://medium.com/@wiemzin/getting-started-with-recursion-schemes-using-matryoshka-f5b5ec01bb

object RecursionTest extends App {
  object Plain {
    sealed trait Matryoshka
    case class Doll(name: String, daughter: Matryoshka) extends Matryoshka
    case class Tiny(name: String) extends Matryoshka

    val dolls = Doll("Anna", Doll("Lisa", Tiny("Kate")))
  }

  object RemoveRecursion {
    sealed trait Matryoshka[T]
    case class Doll[T](name: String, daughter: T) extends Matryoshka[T]
    case class Tiny[T](name: String) extends Matryoshka[T]

    import matryoshka.data.Fix
    import matryoshka.implicits._

    val dolls1: Fix[Matryoshka] = Fix(Doll("Anna", Fix(Doll("Lisa", Fix(Tiny("Kate"))))))

    implicit val dollsFunctor: Functor[Matryoshka] = new Functor[Matryoshka] {
      override def map[A, B](fa: Matryoshka[A])(f: A => B): Matryoshka[B] = fa match {
        case Doll(n, d) => Doll(n, f(d))
        case Tiny(n)    => Tiny(n)
      }
    }

    val coalgebra: Coalgebra[Matryoshka, NonEmptyList[String]] = {
      case NonEmptyList(h, _: INil[String]) => Tiny(h)
      case NonEmptyList(h, l) =>
        val list: List[String] = l.toList
        Doll(h, NonEmptyList(list.head, list.tail: _*))
    }

    val names = NonEmptyList("a", "b", "c", "d")
    val dolls2 = names.ana[Fix[Matryoshka]](coalgebra)

    val algebra: Algebra[Matryoshka, Int] = {
      case Doll(_, daughter) => 1 + daughter
      case Tiny(_)           => 1
    }

    val numDolls = dolls1.cata[Int](algebra)
  }

}
