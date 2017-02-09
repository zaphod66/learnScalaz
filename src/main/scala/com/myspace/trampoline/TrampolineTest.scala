package com.myspace.trampoline

object FirstTrampoline {

  sealed trait Trampoline[+A] {
    final def run: A = this match {
      case Done(v) => v
      case More(k) => k().run
    }
  }

  case class Done[+A](result: A) extends Trampoline[A]
  case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

  case class StateSimple[S, +A](run: S => (A, S)) {
    def map[B](f: A => B) = StateSimple[S, B](s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

    def flatMap[B](f: A => StateSimple[S, B]) = StateSimple[S, B](s => {
      val (a, s1) = run(s)  // yields a stack overflow
      f(a) run s1
    })
  }

  def getStateSimple[S]: StateSimple[S, S]           = StateSimple(s => (s, s))
  def setStateSimple[S](s: S): StateSimple[S, Unit]  = StateSimple(_ => ((), s))
  def pureStateSimple[S, A](a: A): StateSimple[S, A] = StateSimple(s => (a, s))

  def zipIndexSimple[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(
      pureStateSimple[Int, List[(Int, A)]](List.empty[(Int,A)])
    )((acc, a) => for {
      xs <- acc
      n  <- getStateSimple
      _  <- setStateSimple(n + 1)
    } yield (n, a)::xs).run(0)._1.reverse


  case class StateTramp[S, +A](run: S => Trampoline[(A, S)]) {
    def map[B](f: A => B) = flatMap(a => pureStateTramp(f(a)))

    def flatMap[B](f: A => StateTramp[S, B]) = StateTramp[S, B](s => More(() => {
      val (a, s1) = run(s).run
      More(() => f(a) run s1)
    }))
  }

  def getStateTramp[S]: StateTramp[S, S]           = StateTramp(s => Done(s, s))
  def setStateTramp[S](s: S): StateTramp[S, Unit]  = StateTramp(_ => Done((), s))
  def pureStateTramp[S, A](a: A): StateTramp[S, A] = StateTramp(s => Done(a, s))

  def zipIndexTramp[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(
      pureStateTramp[Int, List[(Int, A)]](List.empty[(Int,A)])
    )((acc, a) => for {
      xs <- acc
      n  <- getStateTramp
      _  <- setStateTramp(n + 1)
    } yield (n, a)::xs).run(0).run._1.reverse
}

object MonadicTrampoline {
  sealed trait Trampoline[+A] {
    final def run: A = resume match {
      case Right(a) => a
      case Left(k)  => k().run
    }

    final def resume: Either[() => Trampoline[A], A] = this match {
      case Done(v)       => Right(v)
      case More(k)       => Left(k)
      case FlatMap(a, f) => a match {
        case Done(v)       => f(v).resume
        case More(k)       => Left(() => k() flatMap f)
        case FlatMap(b, g) => b.flatMap((x: Any) => g(x) flatMap f).resume
      }
    }

    final def map[B](f: A => B): Trampoline[B] = flatMap(a => Done(f(a)))

//  final def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = More[B](() => f(run))  // run is not in tail position
    final def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
      case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
      case x             => FlatMap(x, f)
    }

    // cooperative multitasking
    def zip[B](b: Trampoline[B]): Trampoline[(A, B)] = (this.resume, b.resume) match {
      case (Right(a), Right(b)) => Done((a, b))
      case (Right(a), Left(b))  => More(() => Done(a) zip b())
      case (Left(a), Right(b))  => More(() => a() zip Done(b))
      case (Left(a), Left(b))   => More(() => a() zip b())
    }
  }

  case class Done[+A](result: A) extends Trampoline[A]
  case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
  private case class FlatMap[A, +B](sub: Trampoline[A],
                                    k: A => Trampoline[B]) extends Trampoline[B]

  case class State[S, +A](run: S => Trampoline[(A, S)]) {
    def map[B](f: A => B) = flatMap(a => pureState(f(a)))

    def flatMap[B](f: A => State[S, B]) = State[S, B](s =>
      More(() => run(s) flatMap {
        case (a, s1) => More(() => f(a) run s1)
      }))
  }

  def getState[S]: State[S, S]           = State(s => Done(s, s))
  def setState[S](s: S): State[S, Unit]  = State(_ => Done((), s))
  def pureState[S, A](a: A): State[S, A] = State(s => Done(a, s))

  def zipIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(
      pureState[Int, List[(Int, A)]](List.empty[(Int,A)])
    )((acc, a) => for {
      xs <- acc
      n  <- getState
      _  <- setState(n + 1)
    } yield (n, a)::xs).run(0).run._1

  def fib(n: Int): Trampoline[Long] =
    if (n <= 1) Done(n.toLong) else for {
      x <- More(() => fib(n - 1))
      y <- More(() => fib(n - 2))
    } yield x + y
}

object FreeMonad {
  import scala.language.higherKinds
  import scala.language.reflectiveCalls

  sealed trait Free[S[+_], +A] {
    final def map[B](f: A => B): Free[S, B] = flatMap(a => Done(f(a)))

    final def flatMap[B](f: A => Free[S, B]): Free[S, B] = this match {
      case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
      case x             => FlatMap(() => x, f)
    }

    final def run(implicit S: Functor[S], C: Copoint[S]): A = resume match {
      case Right(a) => a
      case Left(k)  => C.get(S.map(k)(_.run))
    }

    final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] = this match {
      case Done(a)     => Right(a)
      case More(k)     => Left(k)
      case a FlatMap f => a() match {
        case Done(a)     => f(a).resume
        case More(k)     => Left(S.map(k)(_ flatMap f))
        case b FlatMap g => b().flatMap((x: Any) => g(x) flatMap f).resume
      }
    }

    final def zip[B](b: Free[S, B])(implicit S: Functor[S]): Free[S, (A, B)] = (this.resume, b.resume) match {
      case (Right(a), Right(b)) => Done((a, b))
      case (Right(a), Left(b))  => More(S.map(b)(y => Done(a) zip y))
      case (Left(a), Right(b))  => More(S.map(a)(x => x zip Done(b)))
      case (Left(a), Left(b))   => More(S.map(a)(x =>
                                   More(S.map(b)(y => x zip y))))
    }
  }

  case class Done[S[+_], +A](a: A) extends Free[S, A]
  case class More[S[+_], +A](k: S[Free[S, A]]) extends Free[S, A]
  private case class FlatMap[S[+_], A, +B](a: () => Free[S, A], f: A => Free[S, B]) extends Free[S, B]

  type Trampoline[+A] = Free[Function0, A]

  trait Functor[F[_]] {
    def map[A, B](m: F[A])(f: A => B): F[B]
  }

  implicit val f0Functor = new Functor[Function0] {
    def map[A, B](a: () => A)(f: A => B): () => B = () => f(a())
  }

  trait Copoint[F[_]] {
    def get[A](fa: F[A]): A
  }

  implicit val f0Copoint = new Copoint[Function0] {
    def get[A](fa: () => A): A = fa()
  }

  // common data types as Free
  type Pair[+A] = (A, A)
  type BinTree[+A] = Free[Pair, A]

  type Tree[+A] = Free[List, A]

  type FreeList[A] = Free[({type l[+B] = (A, B)})#l, Unit]  // that is essentially a FreeMonoid
  type FreeMonoid[A] = Free[({type λ[+α] = (A, α)})#λ, Unit]

  // state monad as Free

  sealed trait StateF[S, +A]
  case class Get[S, A](f: S => A) extends StateF[S, A]
  case class Put[S, A](s: S, a: A) extends StateF[S, A]

  implicit def stateFFunctor[S] = new Functor[({type l[+B] = StateF[S, B]})#l] {
    def map[A, B](m: StateF[S, A])(f: A => B): StateF[S, B] = m match {
      case Get(g)    => Get((s: S) => f(g(s)))
      case Put(s, a) => Put(s, f(a))
    }
  }

  type FreeState[S, +A] = Free[({type λ[+α] = StateF[S,α]})#λ, A]

  def pureState[S, A](a: A): FreeState[S, A] = Done[({type λ[+α] = StateF[S,α]})#λ, A](a)
  def getState[S]: FreeState[S,S] = More[({type λ[+α] = StateF[S,α]})#λ, S](Get(s =>
    Done[({type λ[+α] = StateF[S,α]})#λ, S](s)))
  def setState[S](s: S): FreeState[S,Unit] = More[({type λ[+α] = StateF[S,α]})#λ, Unit](Put(s,
    Done[({type λ[+α] = StateF[S,α]})#λ, Unit](())))

  def eval[S, A](s: S, t: FreeState[S, A]): A = t.resume match {
    case Left(Get(f))    => eval(s, f(s))
    case Left(Put(n, a)) => eval(n, a)
    case Right(a)        => a
  }

  def zipIndex[A](as: List[A]): List[(Int, A)] = eval(0, as.foldLeft(
    pureState[Int, List[(Int, A)]](List.empty[(Int, A)])) {
      (acc, a) => for {
        xs <- acc
        n  <- getState
        _  <- setState(n + 1)
      } yield (n, a)::xs }).reverse
}

object TrampolineTest extends App {

  object testFirst {
    import FirstTrampoline._

    def even[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil => Done(true)
      case _ :: xs => More(() => odd(xs))
    }

    def odd[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil => Done(false)
      case _ :: xs => More(() => even(xs))
    }

    val l1 = List.fill(10000)('a')
    val b1e = even(l1).run
    val b1o = odd(l1).run

    val l2 = List.fill(10001)('a')
    val b2e = even(l2).run
    val b2o = odd(l2).run

    println(s"l1: ${l1.size}: even = $b1e, odd = $b1o")
    println(s"l2: ${l2.size}: even = $b2e, odd = $b2o")

    val z3 = try { zipIndexSimple(List.fill(10)('a')) }    catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z3: $z3")

    val z4 = try { zipIndexSimple(List.fill(10000)('b')) } catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z4: $z4")

    val z5 = try { zipIndexTramp(List.fill(10)('a')) }     catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z3: $z5")

    val z6 = try { zipIndexTramp(List.fill(10000)('b')) }  catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z4: $z6")
  }

  object testSecond {
    import MonadicTrampoline._

    def even[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil => Done(true)
      case _ :: xs => More(() => odd(xs))
    }

    def odd[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil => Done(false)
      case _ :: xs => More(() => even(xs))
    }

    val z1 = try { zipIndex(List.fill(10)('a')) }    catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z3: ${z1.size}")

    val z2 = try { zipIndex(List.fill(10000)('b')) } catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z4: ${z2.size}")

    val n = 20
    println(s"fib($n) = ${fib(n).run}")

    val h = More[Unit](() => Done(print("Hello, ")))
    val w = More[Unit](() => Done(println("World!")))
    val hello: Trampoline[Unit] = for {
      _ <- h
      _ <- w
    } yield ()

    println("==============")
    (hello zip hello).run
    println("==============")
  }

  object testThird {
    import FreeMonad._

    def even[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil => Done(true)
      case _ :: xs => More(() => odd(xs))
    }

    def odd[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil => Done(false)
      case _ :: xs => More(() => even(xs))
    }

    val l1 = List.fill(1000)('a')
    val b1e = even(l1).run
    val b1o = odd(l1).run

    val l2 = List.fill(1001)('a')
    val b2e = even(l2).run
    val b2o = odd(l2).run

    println(s"l1: ${l1.size}: even = $b1e, odd = $b1o")
    println(s"l2: ${l2.size}: even = $b2e, odd = $b2o")

    val z3 = try { zipIndex(List.fill(10)('a')) }    catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z3: $z3")

    val z4 = try { zipIndex(List.fill(10000)('b')) } catch { case _: Throwable => List.empty[(Int, Char)] }
    println(s"z4: $z4")

  }

  testFirst
  println("##########")
  testSecond
  println("##########")
  testThird
}
