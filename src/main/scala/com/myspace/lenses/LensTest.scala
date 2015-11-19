package com.myspace.lenses

import scalaz._
import scala.language.postfixOps

case class City(zip: Int, name: String)
case class Address(city: City, street: String, number: Int)
case class Person(name: String, address: Address)

object LensTest extends App {
  val adrL: Lens[Person, Address] = Lens.lensu((p,a) => p.copy(address = a), _.address)
  val citL: Lens[Address, City]   = Lens.lensu((a,c) => a.copy(city = c), _.city)
  val zipL: Lens[City, Int]       = Lens.lensu((c,z) => c.copy(zip = z), _.zip)

  val pzL = adrL andThen citL andThen zipL

  val p1 = Person("John", Address(City(1000, "City1"), "A-Street", 1))
  val p2 = pzL.mod(1+, p1)

  val zipState = for {
    z <- pzL
    _ <- pzL := z + 1
  } yield z

  println(p1)
  println(p2)
  println(zipState.run(p1))
  println(zipState.eval(p1))

  val f1: Int => Int = x => x + 1
  val f2: Int => Int = x => x * 2
  def f3 = Function.chain(List(f1, f2, f1))

  val x = 2
  println(s"f($x) = ${f1(x)}, f2($x) = ${f2(x)}, f3($x) = ${f3(x)}, ")
}
