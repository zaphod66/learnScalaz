package com.myspace.other

import scala.language.higherKinds

object NaturalTransformationTest extends App {
  trait NatTrans[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  object OptionToList extends NatTrans[Option, List] {
    override def apply[A](fa: Option[A]): List[A] = fa match {
      case None    => List.empty[A]
      case Some(a) => List(a)
    }
  }

  val o1 = Some(1)
  val l1 = OptionToList(o1)
}
