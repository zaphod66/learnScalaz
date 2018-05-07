package com.myspace.parsing

object Parser {
  type ParserFunc[A] = String => List[(A, String)]    // List because of non deterministic grammars

  def success[A](a: A): Parser[A] = {                 // unit / pure
    val run = { s: String => List((a, s))}

    Parser(run)
  }

  def failure[A](): Parser[A] = {
    val run = { _: String => Nil }
    Parser(run)
  }

  def item(): Parser[Char] = {
    val run = { s: String => if (s.isEmpty) Nil else List((s.head, s.tail))}
    Parser(run)
  }

  def item12(): Parser[(Char, Char)] =
//    item().flatMap( a =>
//    item().flatMap( b =>
//    success(a, b)))
    for { a <- item(); b <- item() } yield (a, b)

  def sat(p: Char => Boolean): Parser[Char] =
    item().flatMap(c => if (p(c)) success(c) else failure[Char]())

  def sym(c: Char): Parser[Char] = sat(_ == c)
  def lower(): Parser[Char] = sat(_.isLower)
  def digit(): Parser[Char] = sat(_.isDigit)

  def str(s: String): Parser[String] =
    if (s.isEmpty)
      success("")
    else
      for { _ <- sym(s.head); _ <- str(s.tail) } yield s

  def plus[A](p1: Parser[A], p2: Parser[A]): Parser[A] = {
    val run = { s: String => p1(s) ++ p2(s) }
    Parser(run)
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    plus( for { a <- p; as <- many(p) } yield a::as, success(List.empty[A]))  // success for empty string
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = {
    for { a <- p; as <- many(p) } yield a::as
  }

  def num(): Parser[Int] = {
    for ( digits <- many1(digit()) ) yield
      digits.map(_ - '0').foldLeft(0) {case (n, i) => 10 * n + i}
  }

  def nums(): Parser[List[Int]] = for {
    _  <- sym('[')
    n  <- num()
    ns <- many( for ( _ <- sym(','); x <- num()) yield x)
    _  <- sym(']')
  } yield n::ns
}

import Parser._

case class Parser[A](run: ParserFunc[A]) {
  def apply(s: String) = run(s)

  def map[B](f: A => B): Parser[B] = {
    val runB = { s: String => run(s).map { case (a, rest) => (f(a), rest) } }
    Parser(runB)
  }

  def flatMap[B](f: A => Parser[B]): Parser[B] = {
    val runB = { s: String => run(s).flatMap { case (a, rest) => f(a)(rest) } }
    Parser(runB)
  }
}

