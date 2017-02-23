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

  def collect               = Kleisli[Data, Unit, Raw]( _ => ListT[ErrorOr, Raw](\/-(data)) )
  def format(f: Raw => Frm) = Kleisli[Data, Raw, Frm](a => ListT[ErrorOr, Frm](\/-(List(f(a)))))
  def handle                = Kleisli[Data, Frm, Frm](s => { println(s"handle($s)"); ListT[ErrorOr, Frm](\/-(List(s))) })
  def complete              = Kleisli[Data, Frm, Frm](s => { println(s"complete($s)"); ListT[ErrorOr, Frm](\/-(List(s))) })
}

object ListTransformer extends App {
  object Transform0 {
    type Meta = String
    type Txns = Seq[Transaction]
    type Raw = (Meta, Txns)
    type Frm = (Meta, String)

    def gen: () => List[Raw] = () => List(("Meta1", SampleData.txns1), ("Meta2", SampleData.txns2))
    def f: Txns => String = _.mkString("-")
    def frm: Raw => Frm = r => (r._1, f(r._2))

    val reporter = new Reporter[Raw, Frm](gen, frm)

    val result = reporter.report().run
  }

  object tuple {
    type Meta = (String, Int)
    type Raw = (Meta, Seq[Transaction])
    type Frm = (Meta, String)

    def gen: () => List[Raw] = () => List((("Meta1", 1), SampleData.txns1), (("Meta2", 2), SampleData.txns2))
    def frm: Raw => Frm = r => (r._1, r._2.mkString("-"))
    def nam: Meta => String = m => m._1
    def imp: Meta => Int    = m => m._2

    val reporter = new Reporter[Raw, Frm](gen, frm)

    val result = reporter.report().run
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
    val to3 = to1 map { i => s"-(${i * 2})-" }
    val to4 = OptionT[Task, Int](Task(Option(4)))
    val to5 = to4 map { i => s"-(${i * 2})-" }
    val to6 = OptionT[Task, String](Task(Option("Monad")))
    val to7 = to6 map { i => s"-(${i * 2})-" }
    val to8 = OptionT[Task, String](none[String].point[Task])
    val to9 = to8 map { i => s"-(${i * 2})-" }

    println(s"to2: ${to2.run.run}")
    println(s"to3: ${to3.run.run}")
    println(s"to4: ${to4.run.run}")
    println(s"to5: ${to5.run.run}")
    println(s"to6: ${to6.run.run}")
    println(s"to7: ${to7.run.run}")
    println(s"to8: ${to8.run.run}")
    println(s"to9: ${to9.run.run}")

    type TaskOptionList[T] = ListT[TaskOption, T]
    object TaskOptionList {
      def apply[T](ts: List[T]) = ListT[TaskOption, T](OptionT[Task, List[T]](Task(Option(ts))))
    }

    val toli1 = 1.point[TaskOptionList]
    val toli2 = 2.point[TaskOptionList]
    val toli3 = toli1 ++ toli2
    val toli4 = toli3 map { _ * 2 }
    val toli5 = ListT[TaskOption, Int](OptionT[Task, List[Int]](Task(Option(List(1, 2, 3, 4)))))
    val toli6 = toli5 map { i => s"-<$i>-" }
    val toli7 = TaskOptionList(List('a', 'b', 'c', 'd'))
    val toli8 = toli7 map { c => s"-$c-" }

    println(s"toli3 = ${toli3.run.run.run}")
    println(s"toli4 = ${toli4.run.run.run}")
    println(s"toli5 = ${toli5.run.run.run}")
    println(s"toli6 = ${toli6.run.run.run}")
    println(s"toli7 = ${toli7.run.run.run}")
    println(s"toli8 = ${toli8.run.run.run}")
  }

  println("MonadTransformer")

//Transform0
  Transform1
  Transform2
}
