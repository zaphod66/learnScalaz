package com.myspace.layout

object ExprTest extends App {
  val f = new ExprFormatter

  val e1 = BinOp("*", BinOp("/", Number(1), Number(2)),
                      BinOp("+", Var("x"), Number(1)))

  val e2 = BinOp("+", BinOp("/", Var("x"), Number(2)),
                      BinOp("/", Number(1.5), Var("x")))
  val e3 = BinOp("/", e1, e2)

  val e4 = BinOp("/", BinOp("/", Number(1), Number(2)), Number(3))

  val e5 = BinOp("*", e3, e4)

  val e6 = BinOp("/", Number(1), e5)

  def show(e: Expr): Unit = println(f.format(e) + "\n\n")

  List(e1, e2, e3, e5, e6) foreach show
}
