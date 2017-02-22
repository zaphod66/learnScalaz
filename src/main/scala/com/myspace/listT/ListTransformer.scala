package com.myspace.listT

import scalaz._

import scala.language.higherKinds

case class Transaction(id: Long, orderNumber: String)

object SampleData {
  val txns1 = List(
    Transaction(1, "order1"),
    Transaction(2, "order2"),
    Transaction(3, "order3")
  )
  val txns2 = List(
    Transaction(4, "order4"),
    Transaction(5, "order5"),
    Transaction(6, "order6")
  )
}

trait ReportModule {
  type Error = String
  type ErrorOr[A] = Error \/ A
  type Data[A]    = ListT[ErrorOr, A]

  type Raw     = (String, Seq[Transaction])
  type Frm     = String
  type DataRaw = Data[Raw]
  type DataFrm = Data[Frm]

  def stringify(ts: Raw): Frm = s"""${ts._1} + ${ts._2.mkString("-")}"""

  def collect:               Kleisli[Data, Unit, Raw]
  def format(f: Raw => Frm): Kleisli[Data, Raw, Frm]
  def handle:                Kleisli[Data, Frm, Frm]
  def complete:              Kleisli[Data, Frm, Frm]

  def report = collect andThen format(stringify) andThen handle andThen complete
}

class Reporter extends ReportModule {
  val data1 = ("Meta1", SampleData.txns1)
  val data2 = ("Meta2", SampleData.txns2)

  val ll: Data[Raw] = new ListT[ErrorOr, Raw](\/-(List(data1, data2)))

  def collect                  = Kleisli[Data, Unit, Raw]( _ => ll )
  def format(f: Raw => String) = Kleisli[Data, Raw, Frm](a => new ListT[ErrorOr, Frm](\/-(List(f(a)))))
  def handle                   = Kleisli[Data, Frm, Frm](s => { println(s"handle($s)"); new ListT[ErrorOr, Frm](\/-(List(s))) })
  def complete                 = Kleisli[Data, Frm, Frm](s => { println(s"complete($s)"); new ListT[ErrorOr, Frm](\/-(List(s))) })
}

object ListTransformer extends App {
  object Transform0 {
    val reporter = new Reporter

    val result = reporter.report()

    println(s"result: $result")

    val ass = List(List(1, 2), List(3, 4))

    val ttt = ListT.fromList(ass)

    import Scalaz._

    println(s"ttt            = $ttt")
    println(s"ttt.run        = ${ttt.run}")
    println(s"ttt.headOption = ${ttt.headOption}")
  }

  object Transform1 {
    import Scalaz._

    val ol1 = new ListT[Option, Int](Some(List(1, 2, 3)))
    val ol2 = for {
      i <- ol1
    } yield i * 2

    println(s"ol1: ${ol1.run}")
    println(s"ol2: ${ol2.run}")
  }

  object Transform2 {
    import scalaz.concurrent._
    import Scalaz._

    val l1 = List(1, 2, 3)
    val ol1 = new ListT[Option, Int](Some(List(1, 2, 3)))
    val to1 = new OptionT[Task, Int](Task(Some(1)))

    val tol1 = ol1.point[Task]

    val tol2 = for {
      ol <- tol1
    } yield for {
      i <- ol
    } yield i * 2

    println(s"ol1: ${tol1.run.run}")
    println(s"ol2: ${tol2.run.run}")

    type TaskOption[T] = OptionT[Task, T]
    val to2 = 1.point[TaskOption]
    val to3 = for {
      i <- to1
    } yield s"-(${i * 2})-"

    println(s"to3: ${to3.run.run}")

    type TaskOptionList[T] = ListT[TaskOption, T]
    val toli1 = 1.point[TaskOptionList]
    val toli2 = 2.point[TaskOptionList]
    val toli3 = toli1 ++ toli2
    val toli4 = for {
      i <- toli3
    } yield i * 2

    println(s"toli1 = ${toli1.run.run.run}")
    println(s"toli2 = ${toli2.run.run.run}")
    println(s"toli3 = ${toli3.run.run.run}")
    println(s"toli4 = ${toli4.run.run.run}")

  }

  println("MonadTransformer")

//Transform0
  Transform1
  Transform2
}
