package com.myspace.IoMonad

import scalaz._

import scala.util.Try

sealed trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }
  def withFilter(p: A => Boolean): IO[A] = ???
  def ++(io: IO[Unit]): IO[Unit] = new IO[Unit] {
    def run: Unit = { self.run; io.run }
  }

}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)

  def point[A](a: => A): IO[A] = unit(a)
  def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  def empty: IO[Unit] = new IO[Unit] { def run = () }
}

object IoTest extends App {
  object FirstStep {
    def printFrame(msg: String): IO[Unit] = {
      def printLine(s: String): IO[Unit] = new IO[Unit] { def run: Unit = println(s) }
      def printDeco(s: String): IO[Unit] = new IO[Unit] { def run: Unit = println(s"|$s|") }

      val line = List.fill(msg.length)('-').mkString("+", "", "+")
      printLine(line) ++ printDeco(msg) ++ printLine(line)
    }

    private val c1 = printFrame("Plus")
    private val c2 = printFrame("Quamperfekt")

    c1.run
    c2.run
  }

  object IOUtil {
    def ReadLine: IO[String] = IO { scala.io.StdIn.readLine() }
    def PrintLine(s: String): IO[Unit]  = IO { println(s) }
  }

  object Converter {
    import IOUtil._

    def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

    def converter: IO[Unit] = for {
      _ <- PrintLine("Enter temp in Fahrenheit: ")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(s"$d Fahrenheit = ${fahrenheitToCelsius(d)} Celsius.")
    } yield ()
  }

  import scala.language.higherKinds

  object GuessGame {

    trait Program[F[_]] {
      def finish[A](a: => A): F[A]
      def chain[A, B](fa: F[A], afb: A => F[B]): F[B]
      def map[A, B](fa: F[A], f: A => B): F[B]
    }

    object Program {
      def apply[F[_]](implicit F: Program[F]): Program[F] = F
    }

    implicit class ProgrammSyntax[F[_], A](fa: F[A]) {
      def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)
      def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
    }

    def finish[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)

    trait Console[F[_]] {
      def putStrLn(line: String): F[Unit]
      def getStrLn: F[String]
    }

    object Console {
      def apply[F[_]](implicit F: Console[F]): Console[F] = F
    }

    def putStrLn[F[_]: Console](line: String): F[Unit] = Console[F].putStrLn(line)
    def getStrLn[F[_]: Console]: F[String] = Console[F].getStrLn

    trait Random[F[_]] {
      def nextInt(upper: Int): F[Int]
    }

    object Random {
      def apply[F[_]](implicit F: Random[F]): Random[F] = F
    }

    object Implicits {

      implicit val ProgramIO: Program[IO] = new Program[IO] {
        def finish[A](a: => A): IO[A] = IO { a }
        def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)
        def map[A, B](fa: IO[A], f: A => B): IO[B] = fa.map(f)
      }

      implicit val ConsoleIO: Console[IO] = new Console[IO] {
        override def putStrLn(line: String): IO[Unit] = IO { println(line) }
        override def getStrLn: IO[String] = IO { scala.io.StdIn.readLine }
      }

      implicit val RandomIO: Random[IO] = {
        (upper: Int) => IO { scala.util.Random.nextInt(upper) }
      }

    }

    case class TestData(input: List[String], output: List[String], nums: List[Int]) {
      def putStrLn(line: String): (TestData, Unit) = (copy(output = line :: output), ())
      def getStrLn: (TestData, String) = (copy(input = input.drop(1)), input.head)
      def nextInt(upper: Int): (TestData, Int) = (copy(nums = nums.drop(1)), nums.head)

      def showResults: String = output.reverse.mkString("\n")
    }

    case class TestIO[A](run: TestData => (TestData, A)) { self =>
      def map[B](f: A => B): TestIO[B] = TestIO( t => self.run(t) match { case (t, a) => (t, f(a)) } )
      def flatMap[B](f: A => TestIO[B]): TestIO[B] = TestIO(t => self.run(t) match { case (t, a) => f(a).run(t) })

      def eval(t: TestData): TestData = run(t)._1
    }

    object TestIO {
      def point[A](a: => A): TestIO[A] = TestIO(t => (t, a))
    }

    object TestIOImplicits {

      implicit val ProgramTestIO: Program[TestIO] = new Program[TestIO] {
        def finish[A](a: => A): TestIO[A] = TestIO.point(a)
        def chain[A, B](fa: TestIO[A], afb: A => TestIO[B]): TestIO[B] = fa.flatMap(afb)
        def map[A, B](fa: TestIO[A], f: A => B): TestIO[B] = fa.map(f)
      }

      implicit val ConsoleTestIO: Console[TestIO] = new Console[TestIO] {
        override def putStrLn(line: String): TestIO[Unit] = TestIO( t => t.putStrLn(line) )
        override def getStrLn: TestIO[String] =TestIO( t => t.getStrLn )
      }

      implicit val RandomTestIO: Random[TestIO] = {
        (upper: Int) => TestIO( t => t.nextInt(upper) )
      }

    }

    def nextInt[F[_]](upper: Int)(implicit F: Random[F]): F[Int] = Random[F].nextInt(upper)

    def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

    def checkContinue[F[_]: Program: Console](name: String): F[Boolean] = {
      for {
        _     <- putStrLn(s"Do you want to continue, $name?")
        input <- getStrLn
        cont  <- input match {
            case "y" => finish( true)
            case "n" => finish(false)
            case _ => checkContinue(name)
          }
        } yield cont
    }

    def gameLoop[F[_]: Program: Console: Random](name: String): F[Unit] = {
      for {
        num   <- nextInt(5).map(_ + 1)
        _     <- putStrLn(s"Dear $name, please guess a number from 1 to 5!")
        input <- getStrLn
        _     <- parseInt(input).fold(
                    putStrLn("You did not enter a number.")
                  )(guess =>
                    if (guess == num) putStrLn(s"You guessed right!")
                    else putStrLn(s"You guessed wrong, $name! The number was $num")
                  )
        cont <- checkContinue(name)
        _ <- if (cont) gameLoop(name) else finish { () }
      } yield ()
    }

    def main[F[_]: Program: Console: Random]: F[Unit] =
      for {
        _    <- putStrLn("What is your name?")
        name <- getStrLn
        _    <- putStrLn(s"Hello, $name, welcome to the game.")
        _    <- gameLoop(name)
      } yield ()

    import Implicits._

    def mainIO: IO[Unit] = main[IO]

    import TestIOImplicits._

    def mainTestIO: TestIO[Unit] = main[TestIO]

    val testExample = TestData(
      input  = "John" :: "2" :: "y" :: "1" :: "n" :: Nil,
      output = Nil,
      nums   = 0 :: 0 :: Nil
    )

    def runTest = mainTestIO.eval(testExample).showResults
  }

//  FirstStep
//
//  Converter.converter.run

  println(GuessGame.runTest)

//  GuessGame.mainIO.run
}
