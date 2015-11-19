package com.myspace.pickle

import java.io.FileWriter
/*
import com.myspace.argo

import scala.io.Source
import scala.pickling.Defaults._
import scala.pickling.json._

case class Pers(name: String, age: Int, avg: Float)
case class Price(date: String, open: Float, high: Float, low: Float, close: Float, volume: Int, adjClose: Float)

trait Zero
case class One(a:Int) extends Zero
case class Two(s:String) extends Zero

object PickleTest extends App {
  val p1 = Pers("foo", 21, 114.08f)
  val pvl1 = p1.pickle.value

  println(s"Person: $p1")
  println(s"value : $pvl1")

  val pck2 = JSONPickle(pvl1)
  val p3 = pck2.unpickle[argo.Person]
  val p4 = pvl1.unpickle[argo.Person]

  println(s"pickle: $pck2")
  println(s"Person: $p3")
  println(s"Person: $p4")

  val wire1: String = Two("abc").pickle.value
  val wire2: String = One(42).pickle.value

  println(s"wire1:   $wire1")
  println(s"wire2:   $wire2")

  val w1 = wire1.unpickle[Zero]
  val w2 = wire2.unpickle[Zero]

  println(s"wire1 $w1")
  println(s"wire2 $w2")

  val lines = Source.fromFile("prices_AAPL.csv").getLines.toList drop 1

  val l10 = lines take 10

  val prices1 = l10 map { process(_) }
  val pckles  = prices1 map { _.pickle.value }

  val prices2 = pckles  map { _.unpickle[argo.Price] }

  val fw = new FileWriter("test.txt")
  fw.write("This line overwites file contents!")
  fw.close()

  println("=============")

  prices1 foreach println
  println("-------------")
  println(pckles(0))
  println("-------------")
  prices2 foreach println

  ////////////////////////////////////////////

  private def process(data: String): argo.Price = {
    val vals = data.split(",")

    try {
      argo.Price(date = vals(0),
        open = vals(1).toFloat, high = vals(2).toFloat, low = vals(2).toFloat, close = vals(4).toFloat,
        volume = vals(5).toInt, adjClose = vals(6).toFloat)
    } catch {
      case _: Exception => argo.Price("----", 0.0f, 0.0f, 0.0f, 0.0f, 0, 0.0f)
    }
  }
}
*/