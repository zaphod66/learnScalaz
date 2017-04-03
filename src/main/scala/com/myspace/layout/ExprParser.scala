package com.myspace.layout

import scala.util.parsing.combinator._

object ExprParser extends JavaTokenParsers {
  lazy val expr: Parser[Expr] = term ~ rep("[+-]".r ~ term) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "+" ~ t2) => BinOp("+", t1, t2)
      case (t1, "-" ~ t2) => BinOp("-", t1, t2)
    }
  }

  lazy val term: Parser[Expr] = factor ~ rep("[*/]".r ~ factor) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "*" ~ t2) => BinOp("*", t1, t2)
      case (t1, "/" ~ t2) => BinOp("/", t1, t2)
    }
  }

  lazy val factor: Parser[Expr] = "(" ~> expr <~ ")" | number | variable

  lazy val variable: Parser[Expr] = ".".r ^^ { s => Var(s) }

  lazy val number: Parser[Expr] = floatingPointNumber ^^ { t => Number(t.toDouble)}

  def parse(s: String) = parseAll(expr, s)
}
