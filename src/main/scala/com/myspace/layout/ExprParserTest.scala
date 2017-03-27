package com.myspace.layout

import ExprParser._

import scalaz.{-\/, \/, \/-}

object ExprParserTest extends App {

  def eval(parseResult: ParseResult[Expr]): String \/ Double = {
    def go(expr: Expr): Double = expr match {
      case Number(n)          => n
      case UnOp("-", e)       => -1 * go(e)
      case UnOp("+", e)       => go(e)
      case BinOp("+", e1, e2) => go(e1) + go(e2)
      case BinOp("-", e1, e2) => go(e1) - go(e2)
      case BinOp("*", e1, e2) => go(e1) * go(e2)
      case BinOp("/", e1, e2) => go(e1) / go(e2)
    }

    parseResult match {
      case Success(result, _) => \/-(go(result))
      case Failure(msg, _)    => -\/(msg)
    }
  }

  val f = new ExprFormatter

  val s1 = "1 + 1 * 2"
  val s2 = "1 / (1 - 2 / 3)"
  val s3 = "1 / (1 + (2 * 4 / (3 * 5)))"

  val r1 = parse(s1)
  val r2 = parse(s2)
  val r3 = parse(s3)

  println(s"$s1 = $r1 = ${eval(r1)}")
  println(s"$s2 = $r2 = ${eval(r2)}")
  println(s"$s3 = $r3 = ${eval(r3)}")

  def show(e: Expr): Unit = println(f.format(e) + "\n\n")

  show(r3.get)
}
