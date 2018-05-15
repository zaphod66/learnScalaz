// Jon Pretty 'Lifting data structures to the type level'
// https://www.youtube.com/watch?v=q5I9qoyNePE

package com.myspace.HLists

object MyImpl {
  sealed trait HList
  case class  HCons[H, T <: HList](head: T, tail: T) extends HList
  case object HNil extends HList

  // type level contains
  trait Contains[V, L <: HList]

  // buggy ...
  trait LowerPriority {
    implicit def contains2[V,
                           H,
                           T <: HCons[H, T],
                           L <: HCons[V, T]]
      (implicit ev: Contains[V, T]): Contains[V, L] = null
  }

  object Contains extends LowerPriority {
    implicit def contains1[H, L <: HCons[H, _]]: Contains[H, L] = null
  }

  type X = String
  type Y = Int
  type Z = Double
  //type MyList = X :: Y :: Z :: HNil.type
  type MyList = HCons[X, HCons[Y, HCons[Z, HNil.type]]]

  val s = implicitly[Contains[String, MyList]]
}


object HListTest {
  import MyImpl._

  // value level contains
  def contains[T](list: List[T], value: T): Boolean =
    list match {
      case Nil    => false
      case h :: t => h == value || contains(t, value)
    }

//  type X = String
//  type Y = Int
//  type Z = Double
////type MyList = X :: Y :: Z :: HNil.type
//  type MyList = HCons[X, HCons[Y, HCons[Z, HNil.type]]]
//
//  val s = implicitly[Contains[String, MyList]]
}
