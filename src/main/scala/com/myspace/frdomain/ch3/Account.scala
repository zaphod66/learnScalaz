package com.myspace.frdomain.ch3

import java.util.{Calendar, Date}

object common {
  type Amount = BigDecimal

  val today = Calendar.getInstance.getTime
}

import common._

case class Balance(amount: Amount)

case class Account(no: String, name: String, dateOpen: Date = today, dateClose: Option[Date] = None, balance: Balance = Balance(0))
