package com.myspace.tagless

// https://blog.scalac.io/exploring-tagless-final.html
// https://github.com/pjazdzewski1990/Tagless-final-blog

import scala.language.higherKinds

object TaglessTest extends App {

  object Basic {

    trait Language[Wrapper[_]] {
      def number(v: Int): Wrapper[Int]
      def increment(a: Wrapper[Int]): Wrapper[Int]
      def app(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]

      def text(s: String): Wrapper[String]
      def toUpper(a: Wrapper[String]): Wrapper[String]
      def concat(a: Wrapper[String], b: Wrapper[String]): Wrapper[String]

      def toString(v: Wrapper[Int]): Wrapper[String]
    }

    trait ScalaToLanguageBridge[ScalaValue] {
      def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[ScalaValue]
    }

    def buildNumber(n: Int): ScalaToLanguageBridge[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = L.number(n)
    }

    def buildIncrement(n: Int): ScalaToLanguageBridge[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = L.increment(L.number(n))
    }

    def buildIncrementExpression(e: ScalaToLanguageBridge[Int]): ScalaToLanguageBridge[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = L.increment(e.apply)
    }

    def buildComplexExpression(t: String, a: Int, b: Int): ScalaToLanguageBridge[String] = new ScalaToLanguageBridge[String] {
      override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[String] = {
        val addition = L.app(L.number(a), L.number(b))
        L.concat(L.text(t), L.toString(addition))
      }
    }
  }

  println("-- Tagless final pattern --")
}
