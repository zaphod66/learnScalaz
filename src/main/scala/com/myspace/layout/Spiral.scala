package com.myspace.layout

import Element.elem

object Spiral extends App {
  def spiral(nEdges: Int, direction: Int): Element = {
    val space = elem(" ")
    val corner = elem("+")

    if (nEdges == 1) corner
    else {
      val sp = spiral(nEdges - 1, (direction + 3) % 4)
      val vBar = elem('|', 1, sp.height)
      val hBar = elem('-', sp.width, 1)
      direction match {
        case 0 => (corner beside hBar) above (sp beside space)
        case 1 => (sp above space) beside (corner above vBar)
        case 2 => (space beside sp) above (hBar beside corner)
        case _ => (vBar above corner) beside (space above sp)
      }
    }
  }

  val sp = spiral(12, 0)

  println(sp)
}
