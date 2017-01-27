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
  println("ListTransformer")

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
