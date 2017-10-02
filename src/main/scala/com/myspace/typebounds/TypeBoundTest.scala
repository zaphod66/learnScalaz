package com.myspace.typebounds

// Scala for the Impatient
object TypeBoundTest extends App {
  // 17.3 Bounds for Type Variables

  // T <: S Upper Bound (T is a subtype of S)
  case class Pair1[T <: Comparable[T]](f: T, s: T) {
    def smaller: T = if (f.compareTo(s) <= 0) f else s
  }

  val p11 = Pair1("Fred", "Brooks")
  println(s"""p11.smaller: ${p11.smaller}""")

  class Person(name: String, age: Int) { override def toString = s"Person($name, $age)" }
  class Student(name: String, age: Int, semester: Int) extends Person(name, age) {
    override def toString: String = s"Student($name, $age, $semester)"
  }

  // T >: S Lower Bound (T is a supertype of S)
  case class Pair2[T](f: T, s: T) {
    def replaceFirst1(nf: T) = Pair2(nf, s)
    def replaceFirst2[R >: T](nf: R) = Pair2(nf, s)
  }

  val p21 = Pair2(new Student("f", 21, 1), new Student("s", 22, 2))  // Pair[Student]
  //  val p22 = p214.replaceFirst1(new Person("f", 21))  --> compile error
  val p23 = p21.replaceFirst2(new Person("f", 21))  // Pair[Person]

  println(s"p21: $p21")
  println(s"p23: $p23")

  // 17.4 View Bounds (deprecated !! It's better to replace them with implicit parameters
  // T <% S View Bound (There is an implicit conversion from T to S)
  case class Pair3[T <% Comparable[T]](f: T, s: T)

  // 17.5 Context Bounds
  // T : S (There is an implicit value of type S[T])
  case class Pair4[T : Ordering](f: T, s: T) {
    def smaller(implicit ord: Ordering[T]): T = {
      if (ord.compare(f, s) <= 0) f else s
    }
  }

  val p41 = Pair4(4, 5)
  val p42 = Pair4("Fred", "Brooks")
  println(s"p41.smaller: ${p41.smaller}")
  println(s"p42.smaller: ${p42.smaller}")
}
