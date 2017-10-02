package com.myspace.reader

import scalaz._, Scalaz._

object ReaderScalazTest extends App {
  println("ReaderScalazTest")

  val f1: Int => Int = i => i * 3
  val f2: Int => Int = i => i + 2
  val f3: Int => String = i => i.toString
  val f4 = f1 andThen f2 andThen f3


  val f = Reader((i: Int) => i * 3)
  val g = Reader((i: Int) => i + 2)

  val h = for {
    x <- f
    y <- g
  } yield x + y

  println(s"h: ${h(2)}")

  case class Foo(i: Int)
  case class Bar(s: String)
  case class Config(foo: Foo, bar: Bar)

  val doSomethingWithFoo: Reader[Foo, String] = Reader(foo => "hello " * foo.i)
  val doSomethingWithBar: Reader[Bar, String] = Reader(bar => s"bar is $bar")

  val doSomethingWithConfig: Reader[Config, String] = for {
    resFoo <- doSomethingWithFoo.local((c: Config) => c.foo)
    resBar <- doSomethingWithBar.local((c: Config) => c.bar)
  } yield s"$resFoo $resBar"

  println(s"doSomething: ${doSomethingWithConfig.run(Config(Foo(2), Bar("Bar")))}")

  val rr  = doSomethingWithFoo

  println("End!")
}
