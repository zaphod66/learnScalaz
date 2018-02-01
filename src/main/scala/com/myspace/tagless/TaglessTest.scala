package com.myspace.tagless

// https://blog.scalac.io/exploring-tagless-final.html
// https://github.com/pjazdzewski1990/Tagless-final-blog

import scala.concurrent.Await
import scala.language.higherKinds

object TaglessTest extends App {

  def timed[A](f: => A): (A, Long) = {
    val start = System.currentTimeMillis()
    (f, System.currentTimeMillis() - start)
  }

  object Basic {

    trait Language[Wrapper[_]] {
      def number(v: Int): Wrapper[Int]
      def increment(a: Wrapper[Int]): Wrapper[Int]
      def add(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]

      def text(s: String): Wrapper[String]
      def toUpper(a: Wrapper[String]): Wrapper[String]
      def concat(a: Wrapper[String], b: Wrapper[String]): Wrapper[String]

      def toString(v: Wrapper[Int]): Wrapper[String]
    }

    trait ScalaToLanguageBridge[A] {
      def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[A]
    }

    def buildComplexExpression(t: String, a: Int, b: Int): ScalaToLanguageBridge[String] = new ScalaToLanguageBridge[String] {
      override def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[String] = {
        val inc      = L.increment(L.number(b))
        val addition = L.add(L.number(a), inc)
        L.concat(L.text(t), L.toString(addition))
      }
    }

    val expression: ScalaToLanguageBridge[String] = buildComplexExpression("Result is ", 10, 1)

    def buildIncExpr(n: Int): ScalaToLanguageBridge[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[Int] =
      {
        def go(n: Int, expr: Wrapper[Int]): Wrapper[Int] = {
          if (n == 0)
            expr
          else
            go(n - 1, L.increment(expr))
        }

        go(n, L.number(0))
      }
    }

    type NoWrap[A] = A

    val interpreterNoWrap: Language[NoWrap] = new Language[NoWrap] {
      override def number(v: Int): NoWrap[Int] = v
      override def increment(a: NoWrap[Int]): NoWrap[Int] = a + 1
      override def add(a: NoWrap[Int], b: NoWrap[Int]): NoWrap[Int] = a + b

      override def text(s: String): NoWrap[String] = s
      override def toUpper(a: NoWrap[String]): NoWrap[String] = a.toUpperCase
      override def concat(a: NoWrap[String], b: NoWrap[String]): NoWrap[String] = a + b

      override def toString(v: NoWrap[Int]): NoWrap[String] = v.toString
    }

    val interpreterOption: Language[Option] = new Language[Option] {
      import scalaz.std.option._
      import scalaz.syntax.applicative._

      override def number(v: Int): Option[Int] = Some(v)
      override def increment(a: Option[Int]): Option[Int] = a map { _ + 1 }
      override def add(a: Option[Int], b: Option[Int]): Option[Int] = (a |@| b) { (x, y) => x + y }

      override def text(s: String): Option[String] = Option(s)
      override def toUpper(a: Option[String]): Option[String] = a map { _.toUpperCase }
      override def concat(a: Option[String], b: Option[String]): Option[String] = ( a |@| b ) { (x, y) => x + y }

      override def toString(v: Option[Int]): Option[String] = v map { _.toString }
    }

    type PrettyPrint[ScalaValue] = String

    val interpreterPrettyPrint: Language[PrettyPrint] = new Language[PrettyPrint] {
      override def number(v: Int): PrettyPrint[Int] = s"$v"
      override def increment(a: PrettyPrint[Int]): PrettyPrint[Int] = s"(inc $a)"
      override def add(a: PrettyPrint[Int], b: PrettyPrint[Int]): PrettyPrint[Int] = s"(+ $a $b)"

      override def text(s: String): PrettyPrint[String] = s"[$s]"
      override def toUpper(a: PrettyPrint[String]): PrettyPrint[String] = s"(toUpper $a)"
      override def concat(a: PrettyPrint[String], b: PrettyPrint[String]): PrettyPrint[String] = s"(concat $a $b)"

      override def toString(v: PrettyPrint[Int]): PrettyPrint[String] = s"(toString $v)"
    }

    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    val interpreterFuture: Language[Future] = new Language[Future] {
      override def number(v: Int): Future[Int] = Future.successful(v)
      override def increment(a: Future[Int]): Future[Int] = a map { _ + 1 }
      override def add(a: Future[Int], b: Future[Int]): Future[Int] = for {
        x <- a
        y <- b
      } yield x + y

      override def text(s: String): Future[String] = Future.successful(s)
      override def toUpper(a: Future[String]): Future[String] = a map { _.toUpperCase }
      override def concat(a: Future[String], b: Future[String]): Future[String] = for {
        x <- a
        y <- b
      } yield x + y

      override def toString(v: Future[Int]): Future[String] = v map { _.toString }
    }

    import scalaz.concurrent.Task

    val interpreterTask: Language[Task] = new Language[Task] {
      import scalaz.Nondeterminism

      override def number(v: Int): Task[Int] = Task.now(v)
      override def increment(a: Task[Int]): Task[Int] = a map { _ + 1 }
      override def add(a: Task[Int], b: Task[Int]): Task[Int] = Nondeterminism[Task].mapBoth(a, b)(_ + _)

      override def text(s: String): Task[String] = Task.now(s)
      override def toUpper(a: Task[String]): Task[String] = a map { _.toUpperCase }
      override def concat(a: Task[String], b: Task[String]): Task[String] = Nondeterminism[Task].mapBoth(a, b)(_ + _)

      override def toString(v: Task[Int]): Task[String] = v map { _.toString }
    }
  }

  object Extended {
    import Basic._

    trait LanguageExt[Wrapper[_]] extends Language[Wrapper] {
      def mul(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]
    }

    trait ScalaToLanguageExtBridge[ScalaValue] {
      def apply[Wrapper[_]](L: LanguageExt[Wrapper]): Wrapper[ScalaValue]
    }

    def buildExpression(t: String, a: Int, b: Int, c: Int): ScalaToLanguageExtBridge[String] = new ScalaToLanguageExtBridge[String] {
      override def apply[Wrapper[_]](L: LanguageExt[Wrapper]): Wrapper[String] = {
        val inc = L.increment(L.number(a))
        val mul = L.mul(inc, L.number(b))
        val add = L.add(inc, mul)

        L.concat(L.text(t), L.toString(add))
      }
    }

    val expression: ScalaToLanguageExtBridge[String] = buildExpression("Result is ", 3, 4, 5)

    type NoWrap[ScalaValue] = ScalaValue

    val interpreterNoWrap: LanguageExt[NoWrap] = new LanguageExt[NoWrap] {
      private val basicInterpreter = Basic.interpreterNoWrap

      override def number(v: Int): NoWrap[Int] = basicInterpreter.number(v)
      override def increment(a: NoWrap[Int]): NoWrap[Int] = basicInterpreter.increment(a)
      override def add(a: NoWrap[Int], b: NoWrap[Int]): NoWrap[Int] = basicInterpreter.add(a, b)
      override def mul(a: NoWrap[Int], b: NoWrap[Int]): NoWrap[Int] = a * b

      override def text(s: String): NoWrap[String] = basicInterpreter.text(s)
      override def toUpper(a: NoWrap[String]): NoWrap[String] = basicInterpreter.toUpper(a)
      override def concat(a: NoWrap[String], b: NoWrap[String]): NoWrap[String] = basicInterpreter.concat(a, b)

      override def toString(v: NoWrap[Int]): NoWrap[String] = basicInterpreter.toString(v)
    }

    type PrettyPrint[ScalaValue] = String

    val interpreterPrettyPrint: LanguageExt[PrettyPrint] = new LanguageExt[PrettyPrint] {
      private val basicInterpreter = Basic.interpreterPrettyPrint

      override def number(v: Int): PrettyPrint[Int] = basicInterpreter.number(v)
      override def increment(a: PrettyPrint[Int]): PrettyPrint[Int] = basicInterpreter.increment(a)
      override def add(a: PrettyPrint[Int], b: PrettyPrint[Int]): PrettyPrint[Int] = basicInterpreter.add(a, b)
      override def mul(a: PrettyPrint[Int], b: PrettyPrint[Int]): PrettyPrint[Int] = s"(* $a $b)"

      override def text(s: String): PrettyPrint[String] = basicInterpreter.text(s)
      override def toUpper(a: PrettyPrint[String]): PrettyPrint[String] = basicInterpreter.toUpper(a)
      override def concat(a: PrettyPrint[String], b: PrettyPrint[String]): PrettyPrint[String] =  basicInterpreter.concat(a, b)

      override def toString(v: PrettyPrint[Int]): PrettyPrint[String] = basicInterpreter.toString(v)
    }
  }

  println("-- Tagless final pattern --")

  import scala.concurrent.duration._

  val n = Basic.expression.apply(Basic.interpreterNoWrap)
  val o = Basic.expression.apply(Basic.interpreterOption)
  val p = Basic.expression.apply(Basic.interpreterPrettyPrint)
  val t = Basic.expression.apply(Basic.interpreterTask).run
  val aF = Basic.expression.apply(Basic.interpreterFuture)
  val a = Await.result(aF, 1.second)

  println(s"expression: $n")
  println(s"expression: $o")
  println(s"expression: $p")
  println(s"expression: $a")
  println(s"expression: $t")

  val inc5 = Basic.buildIncExpr(10)
  val inc5n = timed(inc5.apply(Basic.interpreterNoWrap))
  val inc5o = timed(inc5.apply(Basic.interpreterOption))
  val inc5p = timed(inc5.apply(Basic.interpreterPrettyPrint))
  val inc5t = timed(inc5.apply(Basic.interpreterTask).run)
  val inc5f = timed(Await.result(inc5.apply(Basic.interpreterFuture), 100.second))

  println(s"inc5n: $inc5n")
  println(s"inc5o: $inc5o")
  println(s"inc5p: $inc5p")
  println(s"inc5f: $inc5f")
  println(s"inc5t: $inc5t")

  val ne = Extended.expression.apply(Extended.interpreterNoWrap)
  val pe = Extended.expression.apply(Extended.interpreterPrettyPrint)

  println(s"ext expression: $ne")
  println(s"ext expression: $pe")
}
