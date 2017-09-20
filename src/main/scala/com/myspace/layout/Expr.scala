package com.myspace.layout

sealed abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(op: String, arg: Expr) extends Expr
case class BinOp(op: String, l: Expr, r: Expr) extends Expr

object ExprFormatter {
  private val opGroups = Array(
    Set("|", "||"),
    Set("&", "&&"),
    Set("ˆ"),
    Set("==", "!="),
    Set("<", "<=", ">", ">="),
    Set("+", "-"),
    Set("*", "%")
  )

  private val precedence = {
    val assocs = for {
      i <- opGroups.indices
      op <- opGroups(i)
    } yield op -> i

    assocs.toMap
  }

  private val unaryPrecedence = opGroups.length
  private val fractionPrecedence = -1

  private def format(e: Expr, enclPrec: Int): Element = {
    import Element.elem

    e match {
      case Var(name) => elem(name)
      case Number(num) =>
        def stripDot(s: String) = if (s endsWith ".0") s.substring(0, s.length - 2) else s
        elem(stripDot(num.toString))
      case UnOp(op, arg) => elem(op) beside format(arg, unaryPrecedence)
      case BinOp("/", l, r) =>
        val top = format(l, fractionPrecedence)
        val bot = format(r, fractionPrecedence)
        val line = elem('-', top.width max bot.width, 1)
        val frac = top above line above bot
        if (enclPrec != fractionPrecedence) frac
        else elem(" ") beside frac beside elem(" ")
      case BinOp(op, l, r) =>
        val opPrec = precedence(op)
        val lElem = format(l, opPrec)
        val rElem = format(r, opPrec)
        val oper = lElem beside elem(s" $op ") beside rElem
        if (enclPrec <= opPrec) oper
        else elem("(") beside oper beside elem(")")
    }
  }

  def format(e: Expr): Element = format(e, 0)
}
