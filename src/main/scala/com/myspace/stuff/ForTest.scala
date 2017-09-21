package com.myspace.stuff

class Foo[A](val a: A) {
  println(s"Foo($a)")
  def map[B](f: A => B): Foo[B] = {
    println("map -> ")
    val fa = f(a)
    println(s"map ($a -> $fa)")
    val r = new Foo(fa)
    println("<- map")
    r }
  def flatMap[B](f: A => Foo[B]): Foo[B] = {
    println("flatMap -> ")
    val fa = f(a)
    println(s"flatMap ($a -> ${fa.a})")
    println("<- flatMap")
    fa }
}

object ForTest extends App {
  def f(i: Int): Foo[Int] = { def t(x: Int) = x + 3; val r = t(i); println(s"f($i) = $r"); new Foo(r) }
  def g(i: Int): Foo[Int] = { def t(x: Int) = x * 3; val r = t(i); println(s"g($i) = $r"); new Foo(r) }

  val foo = new Foo(1)
  val goo = for {
    a <- foo
    b <- f(a)
    c <- g(b)
  } yield c + 1

  println(s"foo: ${foo.a}, goo: ${goo.a}")
}
