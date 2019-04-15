package com.myspace.learning

import scalaz._, Scalaz._

import scala.language.higherKinds

object day10 extends App {

  println("learning Scalaz day10")

  def myName(step: String): Reader[String, String] = Reader { step + ", I am " + _ }

  def localExample = for {
    a <- myName("1st")
    b <- myName("2nd") >=> Reader { _ + "dy" }
    c <- myName("3rd")
  } yield (a, b, c)

  type ReaderTOption[A, B] = ReaderT[Option, A, B]
/*
  object ReaderTOption extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = kleisli(f)
  }

  def conf(key: String) = ReaderTOption[Map[String, String], String] { _.get(key) }

  def setupConnection = for {
    host <- conf("host")
    user <- conf("user")
    pass <- conf("pass")
  } yield (host, user, pass)

  val conf1 = Map("host" -> "localhost", "user" -> "joe", "pass" -> "****")
  val conf2 = Map("host" -> "localhost", "user" -> "joe")

  // ReaderTOption combines Reader's ability and Option's ability
  val setup1 = setupConnection(conf1) // Some...
  val setup2 = setupConnection(conf2) // None

  // Stacking multiple monad transformers

//  type RTO[C] = ({type l[X] = ReaderTOption[C, X]})#l
//
//  type StateTReaderTOption[C, S, A] = StateT[RTO[C], S, A]
//
//  object StateTReaderTOption extends StateTInstances with StateTFunctions {
//    def apply[C, S, A](f: S => (S, A)) = new StateT[RTO[C], S, A] {
//      def apply(s: S) = f(s).point[RTO[C]]
//    }
//
//    def get[C, S]: StateTReaderTOption[C, S, S] = StateTReaderTOption { s => (s, s) }
//    def put[C, S](s: S): StateTReaderTOption[C, S, Unit] = StateTReaderTOption { _ => (s, ())}
//  }

  type StateTReaderTOption[C, S, A] = StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A]

  object StateTReaderTOption extends StateTInstances with StateTFunctions {
    def apply[C, S, A](f: S => (S, A)) = new StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A] {
      def apply(s: S) = f(s).point[({type l[X] = ReaderTOption[C, X]})#l]
    }

    def get[C, S]: StateTReaderTOption[C, S, S] = StateTReaderTOption { s => (s, s) }
    def put[C, S](s: S): StateTReaderTOption[C, S, Unit] = StateTReaderTOption { _ => (s, ())}
  }

  // Stack like day07

  type Stack = List[Int]
  type Config = Map[String, String]

  val pop1 = StateTReaderTOption[Config, Stack, Int] { case x::xs => (xs, x) }

  val pop2: StateTReaderTOption[Config, Stack, Int] = {
    import StateTReaderTOption.{get, put}

    for {
      s <- get[Config, Stack]
      (x :: xs) = s
      _ <- put(xs)
    } yield x
  }

  def push(x: Int) /*: StateTReaderTOption[Config, Stack, Int] */ = {
    import StateTReaderTOption.{get, put}

    for {
      xs <- get[Config, Stack]
      r  <- put(x :: xs)
    } yield r
  }

  def configure[S](key: String) = new StateTReaderTOption[Config, S, String] {
    def apply(s: S) = ReaderTOption[Config, (S, String)] {
      config: Config => config.get(key) map { (s, _) }
    }
  }

  def stackManip4: StateTReaderTOption[Config, Stack, Int] = for {
    _ <- push(3)
    a <- pop2
    b <- pop2
  } yield b

  val s1 = stackManip4(List(5, 8, 2, 1))(Map())

  def stackManip5: StateTReaderTOption[Config, Stack, Unit] = for {
    x <- configure("x")
    a <- push(x.toInt)
  } yield a

  val s2 = stackManip5(List(5, 8, 2, 1))(Map("x" -> "7"))
*/
}
