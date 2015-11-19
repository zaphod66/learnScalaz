object demo {
  println("Demo")
  def lessThanThirty(x: Int) = {
    println(s"$x? Less than 30")
    x < 30
  }

  def moreThanTwenty(x: Int) = {
    println(s"$x? More than 20")
    x > 20
  }

  val xs = List(1, 25, 40, 5, 23, 0)
  val ys = xs.filter(lessThanThirty)
  val zs = ys.filter(moreThanTwenty)
  zs foreach println
  val ts = try xs.map(1/_)
           catch { case (e: Exception) => Nil }
  ts foreach println
}
