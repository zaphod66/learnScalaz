package com.myspace.layout

import ExprParser._

import scalaz.{-\/, \/, \/-}
//import scalaz._, Scalaz._

object ExprParserTest extends App {

  def eval(parseResult: ParseResult[Expr]): String \/ Double = {
    eval(parseResult, Map.empty[String, Double])
  }

  def eval(parseResult: ParseResult[Expr], env: Map[String, Double]): String \/ Double = {
    type GoResult = String \/ Double

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

  val f = new ExprFormatter

  val s1 = "1 + 1 * 2"
  val s2 = "1 / (1 - 2 / 3)"
  val s3 = "1 / (1 + (2 * 4 / (3 * 5)))"
  val s4 = "1 / 2 / (3 / 4)"
  val s5 = "1 / x"

  val r1 = parse(s1)
  val r2 = parse(s2)
  val r3 = parse(s3)
  val r4 = parse(s4)
  val r5 = parse(s5)

  val m1 = Map("x" -> 1.0)
  val m2 = Map("x" -> 2.0)

  println(s"$s1 = ${r1.get} = ${eval(r1)}")
  println(s"$s2 = ${r2.get} = ${eval(r2)}")
  println(s"$s3 = ${r3.get} = ${eval(r3)}")
  println(s"$s4 = ${r4.get} = ${eval(r4)}")
  println(s"$s5 = ${r5.get} = ${eval(r5)}")
  println(s"$s5 = ${r5.get} = ${eval(r5, m1)}")
  println(s"$s5 = ${r5.get} = ${eval(r5, m2)}")

  def show(e: Expr): Unit = println(f.format(e) + "\n\n")

  show(r3.get)
}
