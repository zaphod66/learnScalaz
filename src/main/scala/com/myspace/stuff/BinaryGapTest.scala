package com.myspace.stuff

import scalaz.State
import scalaz.zio.{App, IO}
import scalaz.zio.console._

/**
  * A binary gap within a positive integer N is any maximal sequence of consecutive zeros that is surrounded by ones
  * at both ends in the binary representation of N. For example,
  * number 9 has binary representation 1001 and contains a binary gap of length 2.
  * The number 529 has binary representation 1000010001 and contains two binary gaps: one of length 4 and one of length 3.
  * The number 20 has binary representation 10100 and contains one binary gap of length 1.
  * The number 15 has binary representation 1111 and has no binary gaps.
  * The number 32 has binary representation 100000 and has no binary gaps.
  *
  * Write a function:
  *   int solution(int N);
  * that, given a positive integer N, returns the length of its longest binary gap.
  * The function should return 0 if N doesn't contain a binary gap.
  * For example, given N = 1041 the function should return 5,
  * because N has binary representation 10000010001 and so its longest binary gap is of length 5.
  * Given N = 32 the function should return 0, because N has binary representation '100000' and thus no binary gaps.
  *
  * Write an efficient algorithm for the following assumptions:
  * N is an integer within the range [1..2,147,483,647].
  */

object BinaryGapTest extends App {
  override def run(args: List[String]): IO[Nothing, ExitStatus] = // IO.point(ExitStatus.ExitNow(0))
    myAppLogic.attempt.map(_.fold(_ => 1, _ => 0)).map(ExitStatus.ExitNow(_))

  case class CurrentState(currentLength: Int, maxLength: Int, started: Boolean)

  def binaryGap(n: Int): Int = {
    def go(iter: Iterable[Char]): State[CurrentState, Iterable[Char]] = State[CurrentState, Iterable[Char]] { s1 =>
      if (iter.isEmpty)
        (s1, iter)
      else if (iter.head == '0')
        if (s1.started)
          go(iter.tail).run(s1.copy(currentLength = s1.currentLength + 1))
        else
          go(iter.tail).run(s1)
      else // if (iter.head == '1')
        if (s1.started) {
          val s2 = if (s1.currentLength > s1.maxLength)
            s1.copy(currentLength = 0, s1.currentLength, started = false)
          else
            s1.copy(currentLength = 0, started = false)

          go(iter.tail).run(s2)
        } else {
          go(iter.tail).run(s1.copy(currentLength = 0, started = true))
        }
    }

    val binaryString = n.toBinaryString
    val iterable     = binaryString.toIterable

    val s = go(iterable).run(CurrentState(currentLength = 0, maxLength = 0, started = false))

    s._1.maxLength
  }

  def myAppLogic: IO[Throwable, Unit] =
    for {
      _ <- putStrLn("Enter number for which the binary gap needs to be computed:")
      l <- getStrLn
      n <- try {
        val i = l.toInt
        IO.point(i)
      } catch {
        case e: Throwable => IO.fail(e)
      }
      g = binaryGap(n)
      _ <- putStrLn(s"$n => $g")
    } yield ()

//List(1041, 529, 32, 20, 15, 9) foreach { i => println(s"$i => ${binaryGap(i)}")}
}
