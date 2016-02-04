package com.myspace.fibonacci

case class Matrix(n: Int, m: Int) {
  def get(i: Int, j: Int): Int = ???
}

object Matrix

object FibTest extends App {



  // using matrix exponentiation TimeComplexity = O(log(n))

  def Matrix_Pow(m: Matrix, p: Int): Matrix = ???

  def fibMatrix(n: Int) = n match {
    case 0 => 0
    case 1 => 1
    case _ => {
      val m = Matrix(2,2) // values {{1, 1}, {1, 0}}
      val res = Matrix_Pow(m, n - 1)

      res.get(0, 0)
    }
  }
}
