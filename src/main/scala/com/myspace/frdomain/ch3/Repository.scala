package com.myspace.frdomain.ch3

import java.util.Date

import scala.util.{Failure, Success, Try}

trait Repository[A, IdType] {
  def query(id: IdType): Try[Option[A]]
  def store(a: A): Try[A]
}

trait AccountRepository extends Repository[Account, String] {
  def query(id: String): Try[Option[Account]]
  def store(a: Account): Try[Account]

  def balance(no: String): Try[Balance] = query(no) match {
    case Success(Some(a)) => Success(a.balance)
    case Success(None)    => Failure(new Exception(s"${this.getClass}: No such Account $no"))
    case Failure(e)       => Failure(e)
  }
  def query(openedOn: Date): Try[Seq[Account]]
}

trait AccountRepositoryInMemory extends AccountRepository {
  lazy val repo = collection.mutable.Map.empty[String, Account]

  def query(no: String): Try[Option[Account]] = Success(repo.get(no))
  def store(a: Account): Try[Account] = {
    repo += (a.no -> a)
    Success(a)
  }
  def query(openedOn: Date): Try[Seq[Account]] = Success(repo.values.filter(_.dateOpen == openedOn).toSeq)
}

object AccountRepositoryInMemory extends AccountRepositoryInMemory
