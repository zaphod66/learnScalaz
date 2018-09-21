package com.myspace.stuff

object TypeSafeContains extends App {

  implicit class ListOps[A](as: List[A]) {
    def safeContains(a: A): Boolean = as.contains(a)
  }

  val xs = List(1, 2, 3)
  val r1 = xs.contains(2)
  val r2 = xs.contains("Hello")

  val r3 = xs.safeContains(2)
//  val r4 = xs.safeContains("Hello")
//  [error] .../TypeSafeContains.scala:14: type mismatch;
//  [error]  found   : String("Hello")
//  [error]  required: Int
//  [error]   val r4 = xs.safeContains("Hello")
//  [error]                            ^
//  [error] one error found
//  [error] (compile:compileIncremental) Compilation failed
}
