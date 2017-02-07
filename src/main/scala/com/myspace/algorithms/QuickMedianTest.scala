package com.myspace.algorithms

object QuickMedianTest extends App {
  // not stack safe
  /**
    * Median calculation using a quick sort like algorithm.
    * !!! Not stacksafe !!!
    * @param l    List[T] for which the median should be calculated
    * @param t2o  implicit ordering
    * @tparam T   Type of elements
    * @return     the Median
    */
  def quickMedian[T](l: List[T])(implicit t2o: T => Ordered[T]): Option[T] = {
    def quickMedianRec(l: List[T], emp: Int): Option[T] = l.headOption flatMap { pivot =>
      val (under, over) = l.tail partition { _ <= pivot }
      if (under.length == emp) Some(pivot)
      else if (under.length > emp) quickMedianRec(under, emp)
      else quickMedianRec(over, emp - under.length - 1)
    }

    quickMedianRec(l, l.length / 2)
  }

  val l1 = (1 to 100).toList
  val l2 = ('A' to 'z').toList

  println(s"Median(l1) = ${quickMedian(l1)}")
  println(s"Median(l2) = ${quickMedian(l2)}")
}
