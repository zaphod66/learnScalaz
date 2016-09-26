package com.myspace.other

case class IO[A](unsafePerformIO: () => A) {
  def map[B](ab: A => B): IO[B] = IO(() => ab(unsafePerformIO()))
  def flatMap[B](afb: A => IO[B]): IO[B] = IO(() => afb(unsafePerformIO()).unsafePerformIO())
  def tryIO(ta: Throwable => A): IO[A] =
    IO(() => IO.tryIO(unsafePerformIO()).unsafePerformIO() match {
      case Left(t) => ta(t)
      case Right(a) => a
    })
}
object IO {
  def point[A](a: => A): IO[A] = IO(() => a)
  def tryIO[A](a: => A): IO[Either[Throwable, A]] =
    IO(() => try { Right(a) } catch { case t : Throwable => Left(t) })
}

case class User(id: Int, name: String, age: Int)

object Database {
  private val users = Map(
    1 -> User(1, "John Doe", 32)
  )

  def getUserById(id: Int): Option[User] = users.get(id)
  def unsafeGetById(id: Int): User = users(id)
}

object IOMonad extends App {
  println("IOMonad")

  val io1 = IO(() => Database.getUserById(1) )
  val ioName1 = io1 map { ou => ou map { u => u.name } }

  println(ioName1.unsafePerformIO())

  val io2 = IO(() => Database.getUserById(2) )
  val ioName2 = io2 map { ou => ou map { u => u.name } }

  println(ioName2.unsafePerformIO())

  val unsafeIo1 = IO.tryIO( Database.getUserById(2) )
  val unsafeIoName = unsafeIo1 map { ou => ou.right map { u => u.map{ _.name } } }

  println(unsafeIoName.unsafePerformIO())
}
