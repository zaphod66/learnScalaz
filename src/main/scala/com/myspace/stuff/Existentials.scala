package com.myspace.stuff

trait Joiner[Elem] {
  type R
  def join(xs: Seq[Elem]): R
}

object Existentials extends App {

  implicit val strJoiner = new Joiner[Char] {
    type R = String

    def join(xs: Seq[Char]): String = xs.mkString
  }

  // the return type of Joiner dose not pollute the signature of doJoin
  def doJoin[T](xs: T*)(implicit j: Joiner[T]): j.R = j.join(xs)

  println(s"doJoin: ${doJoin('a', 'b', 'c')}")
}
