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

trait ReportModule[Raw, Frm] {
  type Error      = String
  type ErrorOr[A] = Error \/ A
  type Data[A]    = ListT[ErrorOr, A]

  def stringify: Raw => Frm

  def collect:               Kleisli[Data, Unit, Raw]
  def format(f: Raw => Frm): Kleisli[Data, Raw, Frm]
  def handle:                Kleisli[Data, Frm, Frm]
  def complete:              Kleisli[Data, Frm, Frm]

  def report = collect andThen format(stringify) andThen handle andThen complete
}

class Reporter[Raw, Frm](generate: () => List[Raw], f: Raw => Frm) extends ReportModule[Raw, Frm] {
  val data = generate()

  def stringify: Raw => Frm = f

  def collect               = Kleisli[Data, Unit, Raw]( _ => new ListT[ErrorOr, Raw](\/-(data)) )
  def format(f: Raw => Frm) = Kleisli[Data, Raw, Frm](a => new ListT[ErrorOr, Frm](\/-(List(f(a)))))
  def handle                = Kleisli[Data, Frm, Frm](s => { println(s"handle($s)"); new ListT[ErrorOr, Frm](\/-(List(s))) })
  def complete              = Kleisli[Data, Frm, Frm](s => { println(s"complete($s)"); new ListT[ErrorOr, Frm](\/-(List(s))) })
}

object ListTransformer extends App {
  println("ListTransformer")

  type Raw = (String, Seq[Transaction])
  type Frm = (String, String)

  def gen: () => List[Raw] = () => List(("Meta1", SampleData.txns1), ("Meta2", SampleData.txns2))
  def frm: Raw => Frm = r => (r._1, r._2.mkString("-"))

  val reporter = new Reporter[Raw, Frm](gen, frm)

  val result = reporter.report().run

  println(s"result: $result")
}
