package com.myspace.logging

import com.myspace.logging.SimpleLogger.Entry

case class SimpleWriter[A](log: List[String], value: A) {

  def map[B](f: A => B): SimpleWriter[B] = SimpleWriter(log, f(value))

  def flatMap[B](f: A => SimpleWriter[B]): SimpleWriter[B] = {
    val nextWriter = f(value)
    SimpleWriter(log ::: nextWriter.log, nextWriter.value)
  }
}

object SimpleLogger {
  case class Entry(msg: String, timeStamp: Long = System.currentTimeMillis()) {
    override def toString: String = {
      val ts = new java.sql.Timestamp(timeStamp)
      s"${ts.toString} - $msg"
    }
  }

  def pure[T](value: T): SimpleLogger[T] = SimpleLogger(Nil, value)
}

case class SimpleLogger[A](log: List[Entry], value: A) {
  def map[B](f: A => B): SimpleLogger[B] = SimpleLogger(log, f(value))

  def flatMap[B](f: A => SimpleLogger[B]) = {
    val nextLogger = f(value)
    SimpleLogger(log ::: nextLogger.log, nextLogger.value)
  }
}

object Logging extends App {
  def writtenAdder(a: Int, b: Int): SimpleWriter[Int] =
    SimpleWriter(List(s"Added $a and $b"), a + b)

  def writtenToString(o: Any): SimpleWriter[String] =
    SimpleWriter(List(s"Converted $o.toString"), o.toString)

  val result1 = for {
    sum <- writtenAdder(2, 3)
    str <- writtenToString(sum)
  } yield s"--> $str <--"

  result1.log.foreach { l => println(s"LOG: $l") }
  println(s"result: ${result1.value}")

  def loggedAdder(a: Int, b: Int): SimpleLogger[Int] =
    SimpleLogger(List(Entry(s"Added $a and $b")), a + b)

  def loggedToString(o: Any): SimpleLogger[String] =
    SimpleLogger(List(Entry(s"Converted $o.toString")), o.toString)

  val result2 = for {
    sum <- loggedAdder(2, 3)
    str <- loggedToString(sum)
  } yield s"--> $str <--"

  result2.log.foreach { l => println(s"LOG: $l") }
  println(s"result: ${result2.value}")
}
