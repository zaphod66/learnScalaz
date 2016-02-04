package com.myspace.typelevel

/////////////////////
// https://www.youtube.com/watch?v=VF-ISUiXIY0 (15:50 min)

trait PNat {
  type N <: PNat
}

class _P0 extends PNat {
  type N = _P0
}

class PSucc[P <: PNat]() extends PNat {
  type N = PSucc[P]
}

/////////////////////

trait Nat
trait Succ[N <: Nat] extends Nat

object Nat {
  class _0 extends Nat
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]

  def toInt[N <: Nat](implicit iv: ToInt[N]) = iv()

  def toInt[A <: Nat, B <: Nat](f: Fac[A])
    (implicit ev: FacAux[A, B], iv: ToInt[B]) = iv()

  def lessThan[A <: Nat, B <: Nat](implicit ev: LessThan[A, B]): Boolean = true
}

trait ToInt[N <: Nat] {
  def apply(): Int
}

object ToInt {
  import Nat._0

  implicit val toInt0 = new ToInt[_0] { def apply() = 0 }

  implicit def toIntN[N <: Nat](implicit iv: ToInt[N]) =
    new ToInt[Succ[N]] { def apply() = iv() + 1 }
}

trait SumAux[A <: Nat, B <: Nat, C <: Nat]

object SumAux {
  import Nat._0

  implicit def sum0[A <: Nat] = new SumAux[A, _0, A] {}   // a + 0 = a

  implicit def sumN[A <: Nat, B <: Nat, C <: Nat]         // a + b' = a' + b
    (implicit ev: SumAux[Succ[A], B, C]) =
    new SumAux[A, Succ[B], C] {}
}

trait MulAux[A <: Nat, B <: Nat, C <: Nat]

object MulAux {
  import Nat._0

  implicit def mul0[A <: Nat] = new MulAux[A, _0, _0] {}  // a * 0 = 0

  implicit def mulN[A <: Nat, B <: Nat, C <: Nat, D <: Nat] // a * b' = d => a * b + a = d
  (implicit evp: MulAux[A, B, C], evs: SumAux[A, C, D]) =
  new MulAux[A, Succ[B], D] {}
}

trait Fac[A <: Nat] {   // helper trait for toInt
  type Out <: Nat
}

object Fac {
  implicit def fact[A <: Nat, B <: Nat](implicit ev: FacAux[A, B]) =
    new Fac[A] { type Out = B }
}

trait FacAux[A <: Nat, B <: Nat]

object FacAux {
  import Nat.{_0, _1}

  implicit val fac0 = new FacAux[_0, _1] {}       // 0! = 1

  implicit def facN[A <: Nat, B <: Nat, C <: Nat] // n'! = n! * n'
  (implicit evf: FacAux[A, B], evp: MulAux[B, Succ[A], C]) =
  new FacAux[Succ[A], C] {}
}

trait LessThan[A <: Nat, B <: Nat]

object LessThan {
  import Nat._0

  implicit def lessThan0[A <: Nat] = new LessThan[_0, Succ[A]] {}
  implicit def lessThanN[A <: Nat, B <: Nat](implicit ev: LessThan[A, B]) =
    new LessThan[Succ[A], Succ[B]] {}
}

object TypeLevelTest extends App {
  import Nat._

  println(toInt[_7])

  implicitly[SumAux[_2, _3, _5]]
  implicitly[SumAux[_3, _4, _7]]

  implicitly[MulAux[_2, _3, _6]]
  implicitly[MulAux[_3, _3, _9]]
  implicitly[MulAux[_4, _2, _8]]

  implicitly[FacAux[_2, _2]]
  implicitly[FacAux[_3, _6]]

  println(toInt(implicitly[Fac[_3]]))
  println(toInt(implicitly[Fac[_4]]))
  println(toInt(implicitly[Fac[_5]]))

  implicitly[LessThan[_2, _3]]

  println(lessThan(implicitly[LessThan[_3, _7]]))
}
