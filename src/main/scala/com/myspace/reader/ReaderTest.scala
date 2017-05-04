package com.myspace.reader

object MyStuff {

  case class Reader[R, A](run: R => A) {
    def map[B](f: A => B): Reader[R, B] =
      Reader(r => f(run(r)))

    def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(run(r)).run(r))
  }

  object Reader {
    def get[A]: Reader[A, A] = Reader(a => a)
    def pure[R, A](a: => A): Reader[R, A] = Reader(_ => a)
  }

}

case class RichFunction1[R, A](r: R => A) {
  def map[B](f: A => B): R => B = r andThen f
  def flatMap[B](f: A => R => B): R => B = a => f(r(a))(a)
}

object RichFunction1 {
  import scala.language.implicitConversions

  implicit def toRichFunction1[A, B](f: A => B): RichFunction1[A, B] = RichFunction1(f)
}

object MyModel {

  case class Account(no: String, name: String, balance: Long)

  sealed trait AccountRepository {
    def store(account: Account): Option[Account]
    def query(no: String): Option[Account]
    def delete(no: String): Unit
  }

//  def doSomething(id: String, repo: AccountRepository): Option[Account] = ???
//  def cdoSomething(id: String): AccountRepository => Option[Account] = ???
//
//  val bla = cdoSomething("jjj")
//  val res = bla(???)

}

object MyService {
  import scala.language.higherKinds

  trait AccountService[F[_, _], R, A] {
    def open(no: String, name: String): F[R, A]
    def close(no: String): F[R, A]
    def credit(no: String, amount: Long): F[R, A]
    def debit(no: String, amount: Long): F[R, A]
  }
}

object Function1Service {
  import MyService._
  import MyModel._

  trait F1AccountService extends AccountService[Function1, AccountRepository, Option[Account]]
}

object Function1Injection {
  import MyModel._
  import Function1Service._
  import RichFunction1._

  object AccountService extends F1AccountService {
    override def open(no: String, name: String): AccountRepository => Option[Account] = { repo =>
      repo.query(no) match {
        case Some(_) => None
        case None    =>
          val account = Account(no, name, balance = 0)
          repo.store(account)
          Some(account)
      }
    }

    override def close(no: String): AccountRepository => Option[Account] = { repo =>
      repo.query(no) match {
        case Some(acc) =>
          repo.delete(no)
          Some(acc)
        case None => None
      }
    }

    override def credit(no: String, amount: Long): AccountRepository => Option[Account] = { repo =>
      repo.query(no) match {
        case Some(acc) =>
          val newAcc = acc.copy(balance = acc.balance + amount)
          repo.delete(no)
          repo.store(newAcc)
          Some(newAcc)
        case None => None
      }
    }

    override def debit(no: String, amount: Long): AccountRepository => Option[Account] = { repo =>
      repo.query(no) match {
        case Some(acc) =>
          val newAcc = acc.copy(balance = acc.balance - amount)
          repo.delete(no)
          repo.store(newAcc)
          Some(newAcc)
        case None => None
      }
    }

    def transfer(from: String, to: String, amount: Long) =
      for {
        _ <- debit(from, amount)
        _ <- credit(to, amount)
      } yield ()

    def chain(no1: String, no2: String, no3: String, amount: Long) =
      for {
        _ <- transfer(no1, no2, amount)
        _ <- transfer(no2, no3, amount)
      } yield ()
  }
}

object ReaderInjection {

  import MyStuff._
  import MyModel._

  trait AccountService {
    def open(no: String, name: String): Reader[AccountRepository, Option[Account]]
    def close(no: String): Reader[AccountRepository, Option[Account]]
    def credit(no: String, amount: Long): Reader[AccountRepository, Option[Account]]
    def debit(no: String, amount: Long): Reader[AccountRepository, Option[Account]]
  }

  object AccountService extends AccountService {
    override def open(no: String, name: String) = Reader { (repo: AccountRepository) =>
      repo.query(no) match {
        case Some(_) => None
        case None =>
          repo.store(Account(no, name, balance = 0))
          Some(Account(no, name, 0))
      }
    }

    override def close(no: String) = Reader { (repo: AccountRepository) =>
      repo.query(no) match {
        case Some(acc) =>
          repo.delete(no)
          Some(acc)
        case None => None
      }
    }

    override def credit(no: String, amount: Long) = Reader { (repo: AccountRepository) =>
      repo.query(no) match {
        case Some(acc) =>
          val newAcc = acc.copy(balance = acc.balance + amount)
          repo.delete(no)
          repo.store(newAcc)
          Some(newAcc)
        case None => None
      }
    }

    override def debit(no: String, amount: Long) = Reader { repo =>
      repo.query(no) match {
        case Some(acc) =>
          val newAcc = acc.copy(balance = acc.balance - amount)
          repo.delete(no)
          repo.store(newAcc)
          Some(newAcc)
        case None => None
      }
    }

//    override def debit(no: String, amount: Long): Reader[AccountRepository, Option[Account]] = for {
//      repo <- Reader.get
//      result <- repo.query(no) match {
//        case Some(acc) =>
//          val newAcc = acc.copy(balance = acc.balance - amount)
//          repo.delete(no)
//          repo.store(newAcc)
//          Some(newAcc)
//        case None => None
//      }
//    } yield result

    def transfer(from: String, to: String, amount: Long): Reader[AccountRepository, Unit] =
      for {
        _ <- debit(from, amount)
        _ <- credit(to, amount)
      } yield ()

    def chain(no1: String, no2: String, no3: String, amount: Long) =
      for {
        _ <- transfer(no1, no2, amount)
        _ <- transfer(no2, no3, amount)
      } yield ()
  }
}

object ReaderTest extends App {
  import MyModel._, ReaderInjection._
//  object ReaderInjectionApp extends AccountService {
//
//  }
}
