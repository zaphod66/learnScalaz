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

    def sequence[R, A](list: TraversableOnce[Reader[R, A]]): Reader[R, TraversableOnce[A]] = {
      Reader(r => for { l <- list } yield l.run(r))
    }
  }

}

case class Function1Monad[R, A](r: R => A) {
  def pure(a: => A): R => A = _ => a
  def map[B](f: A => B): R => B = r andThen f
  def flatMap[B](f: A => R => B): R => B = a => f(r(a))(a)
}

object Function1Monad {
  import scala.language.implicitConversions

  implicit def toRichFunction1[A, B](f: A => B): Function1Monad[A, B] = Function1Monad(f)
}

object MyModel {

  case class Account(no: String, name: String, balance: Long)

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
  import Repository.AccountRepository

  trait F1AccountService extends AccountService[Function1, AccountRepository, Option[Account]]
}

object Function1Injection {
  import MyModel._
  import Function1Service._
  import Function1Monad._

  object AccountService extends F1AccountService {
    override def open(no: String, name: String) = { repo =>
      repo.query(no) match {
        case Some(_) => None
        case None    =>
          val account = Account(no, name, balance = 0)
          repo.store(account)
          Some(account)
      }
    }

    override def close(no: String) = { repo =>
      repo.query(no) match {
        case Some(acc) =>
          repo.delete(no)
          Some(acc)
        case None => None
      }
    }

    override def credit(no: String, amount: Long) = { repo =>
      repo.query(no) match {
        case Some(acc) =>
          val newAcc = acc.copy(balance = acc.balance + amount)
          repo.delete(no)
          repo.store(newAcc)
          Some(newAcc)
        case None => None
      }
    }

    override def debit(no: String, amount: Long) = { repo =>
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

object ReaderService {
  import MyService._
  import MyModel._
  import MyStuff._
  import Repository.AccountRepository

  trait ReaderAccountService extends AccountService[Reader, AccountRepository, Option[Account]]
}


object ReaderInjection {

  import MyStuff._
  import MyModel._
  import ReaderService._
  import Repository.AccountRepository

  trait AccountService extends ReaderAccountService {
    override def open(no: String, name: String) = Reader { (repo: AccountRepository) =>
      repo.query(no) match {
        case Some(_) => { println(s"Account with $no already exists."); None }
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

object Repository {
  import MyModel._

  sealed trait AccountRepository {
    def store(account: Account): Option[Account]
    def query(no: String): Option[Account]
    def delete(no: String): Option[Account]
  }

  trait AccountRepositoryInMemory extends AccountRepository {
    import collection.mutable.{ Map => MMap }

    lazy val repo = MMap.empty[String, Account]

    override def store(account: Account): Option[Account] = {
      repo += ((account.no, account))

      Some(account)
    }

    override def query(no: String): Option[Account] = repo.get(no)

    override def delete(no: String): Option[Account] = repo.remove(no)
  }
}

object ReaderTest extends App {
  import ReaderInjection._
  object ReaderInjectionApp extends AccountService {

    object Repo extends Repository.AccountRepositoryInMemory

    private val program = for {
      _ <- open("a-123", "John Doe")
      _ <- open("a-124", "Eva Luator")
      _ <- open("a-125", "Mike Hammer")

      _ <- credit("a-123", 10000)
      _ <- credit("a-124",  1000)
      _ <- credit("a-125",   100)

      _ <- chain("a-123", "a-124", "a-125", 500)
    } yield ()

    private val command = for {
      _ <- transfer("a-124", "a-123", 200)
      _ <- transfer("a-125", "a-124", 100)
      _ <- chain("a-123", "a-124", "a-125", 500)
    } yield ()

    private val delete = close("a-123")

    program.run(Repo)
    command.run(Repo)
    command.run(Repo)
    command.run(Repo)

    Repo.repo foreach println

    println("=========")

    delete.run(Repo)

    Repo.repo foreach println
  }

  object ReaderSequenceApp {
    import MyStuff._

    val fns = 1 to 10 map { i => (x: Int) => x + i }
    val rds = fns map { f => Reader(f) }
    val rdr = Reader.sequence(rds)
    val res = rdr.run(3).toList

    println(s"res: $res")

    val reader1 = Reader((i: Int) => i * 3)
    val reader2 = Reader((i: Int) => i + 2)

    val readers = List(reader1, reader2)

    val reader3 = Reader.sequence(readers)

    val result3 = reader3.run(3).toList

    println(s"re3: $result3")
  }

  object ReaderNorris {
    import com.myspace.reader.MyStuff.Reader
    import com.myspace.reader.ReaderScalazTest.Config


    def f[A, B]: A => Reader[Config, B] = ???
    def g[B, C]: B => Reader[Config, C] = ???

    def h[A, C]: A => Reader[Config, C] = f andThen g

    // example
    type Host = String
    def path(s: String): Reader[Host, String] = Reader { host => s"http://$host/$s" }

    private val p = path("foo/bar")

    val p1 = p.run("google.com")
    val p2 = p.run("duckduck.go")

    println(s"p1: $p1")
    println(s"p2: $p2")
  }

  ReaderInjectionApp
  println("--------")
  ReaderSequenceApp
  println("--------")
  ReaderNorris
}
