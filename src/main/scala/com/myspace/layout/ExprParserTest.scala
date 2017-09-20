package com.myspace.layout

import ExprParser._

import scalaz.{-\/, \/, \/-}

object ExprParserTest extends App {

  def eval(parseResult: ParseResult[Expr]): String \/ Double = {
    eval(parseResult, Map.empty[String, Double])
  }

  def eval(parseResult: ParseResult[Expr], env: Map[String, Double]): String \/ Double = {
    import scalaz.syntax.applicative._

    def go(expr: Expr): String \/ Double = expr match {
      case Number(n)          => \/-(n)
      case Var(name)          => env.get(name).fold[String \/ Double](-\/(s"<$name> not found"))(v => \/-(v))
      case UnOp("-", e)       => go(e).map(_ * -1)
      case UnOp("+", e)       => go(e)
      case UnOp(op, _)        => -\/(s"unknown operator <$op>")
      case BinOp("+", e1, e2) => (go(e1) |@| go(e2)) { _ + _ }
      case BinOp("-", e1, e2) => (go(e1) |@| go(e2)) { _ - _ }
      case BinOp("*", e1, e2) => (go(e1) |@| go(e2)) { _ * _ }
      case BinOp("/", e1, e2) => (go(e1) |@| go(e2)) { _ / _ }
      case BinOp(op, _, _)    => -\/(s"unknown operator <$op>")
    }

    parseResult match {
      case Success(result, _) => go(result)
      case NoSuccess(msg, _)  => -\/(msg)
    }
  }

  val f = ExprFormatter

  val s1 = "1 + 2 * 3"
  val s2 = "1 / (1 - 2 / 3)"
  val s3 = "1 / (1 + (2 * 4 / (3 * 5)))"
  val s4 = "1 / 2 / (3 / 4)"
  val s5 = "1 / x"
  val s6 = "a * ( a + b )"

  val r1 = parse(s1)
  val r2 = parse(s2)
  val r3 = parse(s3)
  val r4 = parse(s4)
  val r5 = parse(s5)
  val r6 = parse(s6)

  val m1 = Map("x" -> 1.0)
  val m2 = Map("x" -> 2.0)
  val m3 = Map("a" -> 2.0)
  val m4 = Map("a" -> 2.0, "b" -> 3.0)

  println(s"$s1 = ${r1.get} = ${eval(r1)}")
  println(s"$s2 = ${r2.get} = ${eval(r2)}")
  println(s"$s3 = ${r3.get} = ${eval(r3)}")
  println(s"$s4 = ${r4.get} = ${eval(r4)}")
  println(s"$s5 = ${r5.get} = ${eval(r5)}")
  println(s"$s5 = ${r5.get} = ${eval(r5, m1)}")
  println(s"$s5 = ${r5.get} = ${eval(r5, m2)}")
  println(s"$s6 = ${r6.get} = ${eval(r6)}")
  println(s"$s6 = ${r6.get} = ${eval(r6, m3)}")
  println(s"$s6 = ${r6.get} = ${eval(r6, m4)}")

  def show(e: Expr): Unit = println(f.format(e))

  println("----------")
  show(r5.get)
  println("----------")

  val r = 1 to 9
  r foreach { i =>
    val v = i.toDouble / 10.0
    println(s"$v -> ${eval(r5, Map("x" -> v)).getOrElse(0.0)}")
  }
}
