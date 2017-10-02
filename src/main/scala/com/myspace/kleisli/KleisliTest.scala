package com.myspace.kleisli

object KleisliTest extends App {
  import scalaz._
  import Scalaz._
  import Kleisli._


  // Some functions that take simple types and return higher-kinded types

  def toStr(i: Int): Option[String] = Some(i.toString)
  def toInt(s: String): Option[Int] = Some(s.toInt)
  def double(i: Int): Option[Double] = Some(i.toDouble * 2)

  // Lets compose those functions
  def oldSchool(i: Int) =
    for {
      x <- toStr(i)
      y <- toInt(x)
      z <- double(y)
    } yield z

  val o1: Int => String = i => i.toString
  val o2: String => Int = s => s.toInt
  val o3: Int => Double = i => i.toDouble * 3.14

  val o4 = o1 andThen o2 andThen o3

  val f1: Int => Option[String] = i => Some(i.toString)
  val f2: String => Option[Int] = s => Some(s.toInt)
  val f3: Int => Option[Double] = i => Some(i.toDouble * 3.14)

  def f4: Int => Option[Double] = i => for {
    x <- f1(i)
    y <- f2(x)
    z <- f3(y)
  } yield z

  def f5: Int => Option[Double] = kleisli(f1) andThen kleisli(f2) andThen kleisli(f3)
  def f6 = kleisli(f1) >=> kleisli(f2) >=> kleisli(f3)
  def f7 = kleisli(f1) >==> f2 >==> f3

  // Kleisli!
  val funky1 = Kleisli(toStr) >=> Kleisli(toInt) >=> Kleisli(double)
  val funky2 = Kleisli(toStr) >=> Kleisli(toInt) >=> Kleisli(double)
  val funky3 = Kleisli(toStr) >==> toInt >==> double

  println(oldSchool(2))
  println(funky1(2))
  println(funky2(2))
  println(funky3(2))
  println(f4(2))
  println(f5(2))
  println(f6(2))
  println(f7(2))

  /////////////////////////////////////////////////////////////////////
  // from scalaz examples

  println("==============================================================")

  case class Continent(name: String, countries: List[Country] = List.empty)
  case class Country(name: String, cities: List[City] = List.empty)
  case class City(name: String, isCapital: Boolean = false, inhabitants: Int = 20)

  val data: List[Continent] = List(
    Continent("Europe",
      List(
        Country("Germany",
          List(
            City("Hamburg", inhabitants = 11), City("Berlin", inhabitants = 12))))),
    Continent("America",
      List(
        Country("USA",
          List(
            City("Washington", inhabitants = 21), City("New York", inhabitants = 22))))),
    Continent("Asia",
      List(
        Country("India",
          List(City("New Dehli", inhabitants = 31), City("Calcutta", inhabitants = 32))))))

  def continents(n: String): List[Continent] = data.filter(k => k.name.contains(n))
  def countries(continent: Continent): List[Country] = continent.countries
  def cities(country: Country): List[City] = country.cities
  def inhabitants(c: City): Int = c.inhabitants
  def index(i: Int) = data(i).name

  val allCities1 = kleisli(continents) >==> countries >==> cities
  val allCities2 = kleisli(continents) >=> kleisli(countries) >=> kleisli(cities)

  allCities1("America") foreach println

  // =<< takes a monadical structure compatible with the kleislifunction
  // as its parameter and flatmaps the function over this parameter.
  (allCities1 =<< List("Amer", "Asi")) foreach println

  // with map we can map a function B => C over a kleisli function of the
  // structure A => M[B]
  ((allCities1 map inhabitants) =<< List("Amer", "Asi")) foreach println

  val allCitiesByIndex = allCities1 local index

  allCitiesByIndex(1) foreach println
}
