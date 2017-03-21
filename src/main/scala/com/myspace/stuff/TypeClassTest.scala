package com.myspace.stuff

trait Addable[A] {
  def add(that: A): A
}

object TypeClassTest extends App {

  object Overloading {
    def combine(x: Int,    y: Int):    Int    = x + y
    def combine(x: String, y: String): String = x + y

    println(  s"combine(4224, 4224) = ${combine(4224, 4224)}")
    println(s"""combine("22", "44") = ${combine("22", "44")}""")
  }

  object Conversion {
    import scala.language.implicitConversions

    // A must be convertible to Addable[A] (view bounds are deprecated in 2.11)
    def combine[A <% Addable[A]](x: A, y: A): A = x.add(y)

    implicit def intToAddable(x: Int): Addable[Int] = new Addable[Int] {
      override def add(that: Int) = x + that
    }

    implicit def stringToAddable(x: String): Addable[String] = new Addable[String] {
      override def add(that: String) = x + that
    }

    println(  s"combine(4224, 4224) = ${combine(4224, 4224)}")
    println(s"""combine("22", "44") = ${combine("22", "44")}""")
  }

  object TypeClass1 {
    trait Adder[A] {
      def add(x: A, y: A): A
    }

    implicit object IntAdder extends Adder[Int] {
      override def add(x: Int, y: Int) = x + y
    }

    implicit object StringAdder extends Adder[String] {
      override def add(x: String, y: String) = x + y
    }

    def combine1[A](x: A, y: A)(adder: Adder[A]): A = adder.add(x, y)
    def combine2[A](x: A, y: A)(implicit adder: Adder[A]): A = adder.add(x, y)
    def combine3[A](x: A, y: A)(implicit adder: Adder[A]): A = implicitly[Adder[A]].add(x, y)
    def combine4[A: Adder](x: A, y: A): A = implicitly[Adder[A]].add(x, y)  // syntactic suger for combine3

    println(  s"combine1(4224, 4224) = ${combine1(4224, 4224)(IntAdder)}")
    println(s"""combine1("22", "44") = ${combine1("22", "44")(StringAdder)}""")
    println(  s"combine2(4224, 4224) = ${combine2(4224, 4224)}")
    println(s"""combine2("22", "44") = ${combine2("22", "44")}""")
    println(  s"combine3(4224, 4224) = ${combine3(4224, 4224)}")
    println(s"""combine3("22", "44") = ${combine3("22", "44")}""")
    println(  s"combine4(4224, 4224) = ${combine4(4224, 4224)}")
    println(s"""combine4("22", "44") = ${combine4("22", "44")}""")
  }

  object TypeClass2 {
    trait Adder[A] {
      def add(x: A, y: A): A
    }

    object Adder {
      def apply[A: Adder]: Adder[A] = implicitly
    }

    implicit object IntAdder extends Adder[Int] {
      override def add(x: Int, y: Int) = x + y
    }

    implicit object StringAdder extends Adder[String] {
      override def add(x: String, y: String) = x + y
    }

    def combine[A: Adder](x: A, y: A): A = Adder[A].add(x, y)

    println(  s"combine(4224, 4224) = ${combine(4224, 4224)}")
    println(s"""combine("22", "44") = ${combine("22", "44")}""")
  }

  object TypeClass3 {
    trait Semigroup[A] {
      def add(x: A, y: A): A
    }

    object Semigroup {
      def apply[A: Semigroup]: Semigroup[A] = implicitly
    }

    implicit object IntSemigroup extends Semigroup[Int] {
      override def add(x: Int, y: Int) = x + y
    }

    implicit object StringSemigroup extends Semigroup[String] {
      override def add(x: String, y: String) = x + y
    }

    def combine[A: Semigroup](x: A, y: A): A = Semigroup[A].add(x, y)

    println(  s"combine(4224, 4224) = ${combine(4224, 4224)}")
    println(s"""combine("22", "44") = ${combine("22", "44")}""")
  }

  object FromTwoToMany {
    import TypeClass3._

    trait Monoid[A] extends Semigroup[A] {
      def zero: A
    }

    object Monoid {
      def apply[A: Monoid]: Monoid[A] = implicitly
    }

    implicit object IntMonoid extends Monoid[Int] {
      override def zero = 0
      override def add(x: Int, y: Int) = IntSemigroup.add(x, y)
    }

    implicit object StringMonoid extends Monoid[String] {
      override def zero = ""
      override def add(x: String, y: String) = StringSemigroup.add(x, y)
    }

    def aggregate[A: Monoid](xs: Iterable[A]): A = xs.fold(Monoid[A].zero)(Monoid[A].add)

    println(s"""aggregate(Seq(111,222,333)): ${aggregate(Seq(111,222,333))}""")
    println(s"""aggregate(Seq("1","2","3")): ${aggregate(Seq("1","2","3"))}""")
  }

  Overloading
  Conversion
  TypeClass1
  TypeClass2
  TypeClass3
  FromTwoToMany
}
