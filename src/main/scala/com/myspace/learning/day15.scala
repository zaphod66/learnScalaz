package com.myspace.learning

import scalaz._, Scalaz._

object day15 {

  // Arrows
  
  val f = (_: Int) + 1
  val g = (_: Int) * 100

  // the meaning of '<<<' and '>>>' depends an the Arrow
  // in case of functions it's andThen and compose
  val h1 = f >>> g
  val h2 = f <<< g

  val i1 = h1(2)  // 300
  val i2 = h2(2)  // 201

  val h3 = f *** g
  val (i3, i4) = h3(1, 2)

  val h4 = f &&& g
  val (i5, i6) = h4(2)


}
