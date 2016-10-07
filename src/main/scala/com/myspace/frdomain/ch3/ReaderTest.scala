package com.myspace.frdomain.ch3

import java.util.Date

import common._

import scala.util.{Failure, Success, Try}

case class Reader[R, A](run: R => A) {
  def map[B](f: A => B): Reader[R, B] =
    Reader(r => f(run(r)))
  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
    Reader(r => f(run(r)).run(r))
}

trait AccountService[Account, Amount, Balance] {
  def open(no: String, name: String, dateOpen: Option[Date]): Reader[AccountRepository, Try[Account]]
  def close(no: String, closeDate: Option[Date]): Reader[AccountRepository, Try[Account]]
  def debit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]]
  def credit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]]
  def balance(no: String): Reader[AccountRepository, Try[Balance]]

  def transfer(from: String, to: String, amount: Amount): Reader[AccountRepository, Try[(Account, Account)]] = {
    Reader { repo =>
      for {
        b1 <- debit(from, amount).run(repo)
        b2 <- credit(to, amount).run(repo)
      } yield (b1, b2)
    }
  }
}

object AccountService extends AccountService[Account, Amount, Balance] {

  def open(no: String, name: String, dateOpen: Option[Date]): Reader[AccountRepository, Try[Account]] = Reader { repo =>
    repo.query(no) match {
      case Success(Some(a)) => Failure(new Exception(s"Account already exists $no"))
      case Success(None)    =>
        if (no.isEmpty || name.isEmpty) Failure(new Exception("Account no and name must not be blank"))
        else if (dateOpen.getOrElse(today) before today) Failure(new Exception(s"Cannot open account in the past $dateOpen < $today"))
        else repo.store(Account(no, name, dateOpen.getOrElse(today)))
      case Failure(e)       => Failure(new Exception("open", e))
    }
  }
  def close(no: String, closeDate: Option[Date]): Reader[AccountRepository, Try[Account]] = Reader { repo =>
    repo.query(no) match {
      case Success(Some(a)) =>
        if (closeDate.getOrElse(today) before a.dateOpen)
          Failure(new Exception(s"closeDate before openDate ($closeDate < ${a.dateOpen}"))
        else
          repo.store(a.copy(dateClose = closeDate))
      case Success(None)    => Failure(new Exception(s"close: Account not found: $no"))
      case Failure(e)       => Failure(new Exception("close", e))
    }
  }

  def debit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]] = Reader { repo =>
    repo.query(no) match {
      case Success(Some(a)) =>
        if (a.balance.amount < amount)
          Failure(new Exception(s"Insufficient balance for $amount"))
        else
          repo.store(a.copy(balance = Balance(a.balance.amount - amount)))
      case Success(None)    => Failure(new Exception(s"debit: Account not found: $no"))
      case Failure(e)       => Failure(new Exception("debit", e))
    }
  }

  def credit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]] = Reader { repo =>
    repo.query(no) match {
      case Success(Some(a)) => repo.store(a.copy(balance = Balance(a.balance.amount + amount)))
      case Success(None)    => Failure(new Exception(s"credit: Account not found: $no"))
      case Failure(e)       => Failure(new Exception("credit", e))
    }
  }

  def balance(no: String): Reader[AccountRepository, Try[Balance]] = Reader { repo => repo.balance(no) }
}

object ReaderTest extends App {
  import AccountService._

  def op1(no: String, name: String) = {
    for {
      _ <- open(no, name, None)
      _ <- credit(no, BigDecimal(50))
      b <- balance(no)
    } yield b
  }

  def op2(no: String) = {
    for {
      _ <- credit(no, BigDecimal(100))
      _ <- credit(no, BigDecimal(200))
      _ <- debit(no, BigDecimal(100))
      b <- balance(no)
    } yield b
  }

  val repo = AccountRepositoryInMemory

  val b1 = op1("1", "John Doe").run(repo)
  val b2 = op1("2", "Eva Luator").run(repo)
  val b3 = op1("1", "John Doe").run(repo)
  val a1 = transfer("1", "2", BigDecimal(25)).run(repo)
  val a2 = transfer("2", "1", BigDecimal(100)).run(repo)
  val a3 = transfer("2", "1", BigDecimal(25)).run(repo)
  val t1 = op2("1").run(repo)
  val t2 = op2("4").run(repo)

  println(s"b1: $b1")
  println(s"b2: $b2")
  println(s"b3: $b3")
  println(s"a1: $a1")
  println(s"a2: $a2")
  println(s"a3: $a3")
  println(s"t1: $t1")
  println(s"t2: $t2")
}
