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

    def buildNumber(v: Int): ScalaToLanguageBridge[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[Int] = L.number(v)
    }

    def buildAdd(e1: ScalaToLanguageBridge[Int], e2: ScalaToLanguageBridge[Int]): ScalaToLanguageBridge[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[Int] = L.add(e1.apply(L), e2.apply(L))
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

    def buildAddTree(n: Int): ScalaToLanguageBridge[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[Int] = {
        def go(n: Int, expr: Wrapper[Int]): Wrapper[Int] = {
          if (n == 0)
            expr
          else {
            go(n - 1, L.add(expr, expr))
          }
        }

        go(n, L.number(1))
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

    type PrettyPrint[A] = String

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

    import scalaz.concurrent.{ Task => Taskz }

    val interpreterTaskz: Language[Taskz] = new Language[Taskz] {
      import scalaz.Nondeterminism

      override def number(v: Int): Taskz[Int] = Taskz.now(v)
      override def increment(a: Taskz[Int]): Taskz[Int] = a map { _ + 1 }
      override def add(a: Taskz[Int], b: Taskz[Int]): Taskz[Int] = Nondeterminism[Taskz].mapBoth(a, b)(_ + _)

      override def text(s: String): Taskz[String] = Taskz.now(s)
      override def toUpper(a: Taskz[String]): Taskz[String] = a map { _.toUpperCase }
      override def concat(a: Taskz[String], b: Taskz[String]): Taskz[String] = Nondeterminism[Taskz].mapBoth(a, b)(_ + _)

      override def toString(v: Taskz[Int]): Taskz[String] = v map { _.toString }
    }

    import monix.eval.{ Task => Taskm }

    val interpreterTaskm: Language[Taskm] = new Language[Taskm] {
      override def number(v: Int): Taskm[Int] = Taskm.now(v)
      override def increment(a: Taskm[Int]): Taskm[Int] = a map { _ + 1 }
      override def add(a: Taskm[Int], b: Taskm[Int]): Taskm[Int] = Taskm.mapBoth(a, b)(_ + _)

      override def text(s: String): Taskm[String] = Taskm.now(s)
      override def toUpper(a: Taskm[String]): Taskm[String] = a map { _.toUpperCase }
      override def concat(a: Taskm[String], b: Taskm[String]): Taskm[String] = Taskm.mapBoth(a, b)( _ + _ )

      override def toString(v: Taskm[Int]): Taskm[String] = v map { _.toString }
    }

    type Nested[A] = ScalaToLanguageBridge[A]

    val interpreterSimplify: Language[Nested] = new Language[Nested] {
      var nesting = 0

      override def number(v: Int): Nested[Int] = new ScalaToLanguageBridge[Int] {
        override def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[Int] = {
          if (nesting > 0) {
            val temp = nesting
            nesting = 0

            L.add(L.number(temp), L.number(v))
          } else {
            L.number(v)
          }
        }
      }

      override def increment(a: Nested[Int]): Nested[Int] = new ScalaToLanguageBridge[Int] {
        override def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[Int] = {
          nesting = nesting + 1
          a.apply(L)
        }
      }

      override def add(a: Nested[Int], b: Nested[Int]): Nested[Int] = new ScalaToLanguageBridge[Int] {
        override def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[Int] = {
          if (nesting > 0) {
            val temp = nesting
            nesting = 0

            L.add(L.number(temp), L.add(a.apply(L), b.apply(L)))
          } else {
            L.add(a.apply(L), b.apply(L))
          }
        }
      }

      override def text(s: String): Nested[String] = ???
      override def toUpper(a: Nested[String]): Nested[String] = ???
      override def concat(a: Nested[String], b: Nested[String]): Nested[String] = ???

      override def toString(v: Nested[Int]): Nested[String] = ???
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
  import monix.execution.Scheduler.Implicits.global

  val n = Basic.expression.apply(Basic.interpreterNoWrap)
  val o = Basic.expression.apply(Basic.interpreterOption)
  val p = Basic.expression.apply(Basic.interpreterPrettyPrint)
  val f = Await.result(Basic.expression.apply(Basic.interpreterFuture), 1.second)
  val t = Basic.expression.apply(Basic.interpreterTaskz).run
  val m = Await.result(Basic.expression.apply(Basic.interpreterTaskm).runAsync, 1.second)

  println(s"expression n: $n")
  println(s"expression o: $o")
  println(s"expression p: $p")
  println(s"expression f: $f")
  println(s"expression t: $t")
  println(s"expression m: $m")

//  val inc10 = Basic.buildIncExpr(10)
//  val inc10n = timed(inc10.apply(Basic.interpreterNoWrap))
//  val inc10o = timed(inc10.apply(Basic.interpreterOption))
//  val inc10p = timed(inc10.apply(Basic.interpreterPrettyPrint))
//  val inc10f = timed(Await.result(inc10.apply(Basic.interpreterFuture), 100.second))
//  val inc10t = timed(inc10.apply(Basic.interpreterTaskz).run)
//  val inc10m = timed(Await.result(inc10.apply(Basic.interpreterTaskm).runAsync, 100.second))
//
//  println(s"inc10n: $inc10n")
//  println(s"inc10o: $inc10o")
//  println(s"inc10p: $inc10p")
//  println(s"inc10f: $inc10f")
//  println(s"inc10t: $inc10t")
//  println(s"inc10m: $inc10m")

//  val incS  = inc10.apply(Basic.interpreterSimplify)
//  val incSn = incS.apply(Basic.interpreterNoWrap)
//  val incSp = incS.apply(Basic.interpreterPrettyPrint)
//
//  println(s"incSn: $incSn")
//  println(s"incSp: $incSp")

//  val addExpr = Basic.buildAdd(inc10, inc10)
//  val addEn   = addExpr.apply(Basic.interpreterNoWrap)
//  val addEp   = addExpr.apply(Basic.interpreterPrettyPrint)
//
//  val addES  = addExpr.apply(Basic.interpreterSimplify)
//  val addESn = addES.apply(Basic.interpreterNoWrap)
//  val addESp = addES.apply(Basic.interpreterPrettyPrint)
//
//  println(s"addEn: $addEn")
//  println(s"addEp: $addEp")
//  println(s"addESn: $addESn")
//  println(s"addESp: $addESp")

  val inc200 = Basic.buildIncExpr(200)
  val add200 = Basic.buildAdd(inc200, inc200)

  val add200f = timed(Await.result(add200.apply(Basic.interpreterFuture), 100.seconds))
  val add200t = timed(add200.apply(Basic.interpreterTaskz).run)
  val add200m = timed(Await.result(add200.apply(Basic.interpreterTaskm).runAsync, 100.seconds))

  println(s"add200f: $add200f")
  println(s"add200t: $add200t")
  println(s"add200m: $add200m")

  val addTree = Basic.buildAddTree(20)

  val addTree200p = timed(addTree.apply(Basic.interpreterPrettyPrint))
  val addTree200f = timed(Await.result(addTree.apply(Basic.interpreterFuture), 100.seconds))
  val addTree200t = timed(addTree.apply(Basic.interpreterTaskz).run)
  val addTree200m = timed(Await.result(addTree.apply(Basic.interpreterTaskm).runAsync, 100.seconds))

  println(s"addTree200p: ${addTree200p._2}")
  println(s"addTree200f: $addTree200f")
  println(s"addTree200t: $addTree200t")
  println(s"addTree200m: $addTree200m")

//  val ne = Extended.expression.apply(Extended.interpreterNoWrap)
//  val pe = Extended.expression.apply(Extended.interpreterPrettyPrint)
//
//  println(s"ext expression: $ne")
//  println(s"ext expression: $pe")
}
