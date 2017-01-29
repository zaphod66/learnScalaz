// see http://polygonalhell.blogspot.de/2014/12/scalaz-getting-to-grips-free-monad.html

package com.myspace.free

import scalaz.Scalaz._
import scalaz._

sealed trait ForthOperators[A]

final case class Push[A](value: Int, o: A) extends ForthOperators[A]
final case class Add[A](o: A) extends ForthOperators[A]
final case class Mul[A](o: A) extends ForthOperators[A]
final case class Dup[A](o: A) extends ForthOperators[A]
final case class End[A](o: A) extends ForthOperators[A]

object FreeForth extends App {

  // The Free Monad basically provide a Monad given a Functor, so the next thing we need to provide is our Functor

  implicit val ForthFunctor: Functor[ForthOperators] = new Functor[ForthOperators] {
    def map[A, B](fa: ForthOperators[A])(f: A => B): ForthOperators[B] = fa match {
      case Push(value, cont) => Push(value, f(cont))
      case Add(cont)         => Add(f(cont))
      case Mul(cont)         => Mul(f(cont))
      case Dup(cont)         => Dup(f(cont))
      case End(cont)         => End(f(cont))
    }
  }

  // We can now use the Free.LiftF function to convert our functor into a Free Monad

  type ForthProgram[A] = Free[ForthOperators, A]

  import scala.language.implicitConversions

  implicit def liftForth[A](forth: ForthOperators[A]): ForthProgram[A] = Free.liftF(forth)

  // the type we are wrapping (o: A) in the Monad is actually Unit because in this case we don't care about the returned value
  def push(value: Int) = Push(value, ())
  def add = Add(())
  def mul = Mul(())
  def dup = Dup(())
  def end = End(())

  val square = for {
    _ <- dup
    _ <- mul
  } yield ()

  val testProgram = for {
    _ <- push(3)
    _ <- push(6)
    _ <- add
    _ <- square
  } yield ()

  /////////////////////////////////////////////////////////

  // Run the program

  // ----------------------------------

  // by hand, not tail recursive
  final def runForthProgram(stack: List[Int], program: ForthProgram[Unit]): List[Int] = program.fold(
    _ => stack,
    {
      case Push(value, cont) => runForthProgram(value :: stack, cont)
      case Add(cont) =>
        val a :: b :: tail = stack
        runForthProgram((a + b) :: tail, cont)
      case Mul(cont) =>
        val a :: b :: tail = stack
        runForthProgram((a * b) :: tail, cont)
      case Dup(cont) =>
        val a :: tail = stack
        runForthProgram(a :: a :: tail, cont)
      case End(_) =>
        stack
    }
  )

  println(runForthProgram(Nil, testProgram))

  // ----------------------------------

  // "foldRun" which is tail recursive

  def runFn(stack: List[Int], program: ForthOperators[ForthProgram[Unit]]): (List[Int], ForthProgram[Unit]) =
    program match {
      case Push(value, cont) => (value :: stack, cont)
      case Add(cont) =>
        val a :: b :: tail = stack
        ((a + b) :: tail, cont)
      case Mul(cont) =>
        val a :: b :: tail = stack
        ((a * b) :: tail, cont)
      case Dup(cont) =>
        val a :: tail = stack
        (a :: a :: tail, cont)
      case End(_) =>
        (stack, Free.point())
    }

  println(testProgram.foldRun(List.empty[Int])(runFn))

  // ----------------------------------

  // Natural Transformation (map one Functor type to another (here Free Monad -> some other Functor)
  // So what other functor?
  // Well to do anything useful we need to carry our stack around, and we know the functor needs to wrap () and not our stack.
  // So we could either implement one or we can use the State Monad that's designed for exactly this purpose.

  type Stack = List[Int]
  type StackState[A] = State[Stack, A]

  def runTransform: ForthOperators ~> StackState = new (ForthOperators ~> StackState) {
    override def apply[A](op: ForthOperators[A]): StackState[A] = op match {
      case Push(value, cont) =>
        State((stack: Stack) => (value :: stack, cont))
      case Add(cont) =>
        State((stack: Stack) => {
          val a :: b :: tail = stack
          ((a + b) :: tail, cont)
        })
      case Mul(cont) =>
        State((stack: Stack) => {
          val a :: b :: tail = stack
          ((a * b) :: tail, cont)
        })
      case Dup(cont) =>
        State((stack: Stack) => {
          val a :: tail = stack
          (a :: a :: tail, cont)
        })
      case End(cont) =>
        // this doesn't work with natural transformations
        State((stack: Stack) => (stack, cont))
    }
  }

  // the call to foldMap transforms the FreeMonads into StateMonads.
  // We call exec to run the StateMonads and provide an initial Stack
  println(testProgram.foldMap(runTransform).exec(List.empty[Int]))

  def printTransform: ForthOperators ~> Id = new (ForthOperators ~> Id) {
    override def apply[A](fa: ForthOperators[A]): Id[A] = fa match {
      case Push(value, cont) => println(s"Push($value)"); cont
      case Add(cont)         => println("Add");           cont
      case Mul(cont)         => println("Mul");           cont
      case Dup(cont)         => println("Dup");           cont
      case End(cont)         => println("End");           cont
    }
  }

  println("==========")
  testProgram.foldMap(printTransform)

  // ----------------------------------

  object Coyoneda {
    // Using Coyoneda (you don't have to provide a Functor any longer

    type ForthProgramC[A] = Free.FreeC[ForthOperators, A]

    implicit def liftForthC[A](forth: ForthOperators[A]): ForthProgramC[A] = Free.liftFC(forth)
  }
}
