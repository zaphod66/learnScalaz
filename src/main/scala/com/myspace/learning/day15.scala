package com.myspace.learning

import scalaz._, Scalaz._

import scala.language.higherKinds
import scala.language.postfixOps

object day15 extends App {

  // Arrows

  val f = (_: Int) + 1
  val g = (_: Int) * 100
  val r = (_: String) reverse

  // the meaning of '<<<' and '>>>' depends an the Arrow
  // in case of functions it's andThen and compose
  val h1 = f >>> g
  val h2 = f <<< g

  val i1 = h1(2)  // 300
  val i2 = h2(2)  // 201

  val h3 = f *** g
  val (i3, i4) = h3(1, 2)

  val h4 = f &&& g
  val (i5, i6) = h4(2)

  // Function1 Arrow
  {
    val f1 = f.first apply (7, "abc")
    val f2 = f.second apply ("def", 8)
    val f3 = f *** r apply (7, "ghi")
    val f4 = f &&& g apply 7
    val f5 = f.product apply (10, 20)

    println(s"f1: $f1")
    println(s"f2: $f2")
    println(s"f3: $f3")
    println(s"f4: $f4")
    println(s"f5: $f5")
  }

  // Endomorphism
  {
    import Kleisli._

    def applyX10[Arr[_, _]: Category, A](f: Arr[A, A]) = List.fill(10)(Endomorphic(f)).suml

    def f = (_: Int) + 1
    def k = kleisli((i: Int) => (i % 2 === 0).option(i * 2))

    val f1 = applyX10(f).run(1)
    val f2 = applyX10[=?>, Int](k).run(2)

    println(s"f1: $f1")
    println(s"f2: $f2")
  }

  // Kleisli Arrow
  {
    import Kleisli._

    val k = kleisli((n: List[Int]) => (!n.isEmpty).option(n map { _.toString.reverse }))
    val s = kleisli((n: Int) => (!(n % 7 == 0)).option(n * 4))
    val t = kleisli((n: Int) => (n <= 100).option(n * 13))

    val l = List(1, 12, 123)

    val f1 = k.first apply (l, "abc")
    val f2 = k.first apply (Nil, "abc")
    val f3 = k.second apply ("def", l)
    val f4 = k.second apply ("def", Nil)

    val p = k *** s   // Combine k and s on the Kleisli arrow using Option

    val f5 = p apply (l, 18)
    val f6 = p apply (l, 14)
    val f7 = p apply (Nil, 18)

    val q = s &&& t   // Perform both s and t on one value on Kleisli arrow using Option

    val q1 = q apply 3
    val q2 = q apply 7
    val q3 = q apply 101

    val j = k.product
    val k1 = j apply (l, l)
    val k2 = j apply (Nil, l)
    val k3 = j apply (l, Nil)

    println(s"f1: $f1")
    println(s"f2: $f2")
    println(s"f3: $f3")
    println(s"f4: $f4")
    println(s"f5: $f5")
    println(s"f6: $f6")
    println(s"f7: $f7")
    println(s"q1: $q1")
    println(s"q2: $q2")
    println(s"q3: $q3")
    println(s"k1: $k1")
    println(s"k2: $k2")
    println(s"k3: $k3")
  }

  // Cokleisli Arrow
  {
    import NonEmptyList._

    val sum = Cokleisli((n: NonEmptyList[Int]) => n.foldLeft(0)((a, i) => a + i))
    val min = Cokleisli((n: NonEmptyList[Int]) => n.minimum1)

    val nums = nels(1, 2, 3)

    val f1 = nums.cojoin
    val f2 = sum(nums)
    val f3 = min(nums)

    val f = sum &&& min

    val f4 = f(nums)

    println(s"f1: $f1")
    println(s"f2: $f2")
    println(s"f3: $f3")
    println(s"f4: $f4")
  }

  // Unapply

//val a1 = Applicative[Function1[Int, Int]] // => error takes no type parameter, expected one
  val a1 = Applicative[({type l[A] = Int => A})#l]
  val u1 = implicitly[Unapply[Applicative, Int => Int]]
  val u2 = implicitly[Unapply[Applicative, Int]]

  val failedTree: Tree[Validation[String, Int]] = 1.success[String].node(
    2.success[String].leaf, "boom".failure[Int].leaf)

  val f1 = failedTree.sequence[({type l[X]=Validation[String, X]})#l, Int]
  val f2 = failedTree.sequenceU

}
