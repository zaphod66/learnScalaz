package com.myspace.HLists

object MyImpl {
  sealed trait HList
  case class  HCons[H, T <: HList](head: T, tail: T) extends HList
  case object HNil extends HList

  // type level contains
  trait Contains[V, L <: HList]

  trait LowerPriority {
    implicit def contains2[V,
                           H,
                           T <: HList,
                           L <: HCons[H, T]]
      (implicit ev: Contains[V, T]): Contains[V, L] = null
  }

  object Contains extends LowerPriority {
    implicit def contains1[H, L <: HCons[H, _]]: Contains[H, L] = null
  }
}


object HListTest {
  // value level contains
  def contains[T](list: List[T], value: T): Boolean =
    list match {
      case Nil    => false
      case h :: t => h == value || contains(t, value)
    }
}
