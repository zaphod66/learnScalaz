package com.myspace.other

object LearnHaskellChapter09 {
  type CupType = (Int => Int) => Int

  def Cup: Int => CupType = amount => f => f(amount)

  def getAmount(cup: CupType) = cup(identity[Int])

  def drink(cup: CupType, nip: Int) = {
    val oldAmount = getAmount(cup)
    val diff      = oldAmount - nip

    if (diff >= 0)
      Cup(diff)
    else
      Cup(0)
  }
}
