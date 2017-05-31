package com.myspace.streams

import akka.actor.Actor
import akka.stream.actor.ActorSubscriberMessage.OnNext
import akka.stream.actor.{ActorSubscriber, MaxInFlightRequestStrategy}

import scala.concurrent.blocking
import scala.collection.mutable.{ Map => MMap }

import scalaz._
import Scalaz._

class Summarizer extends Actor with ActorSubscriber with Logging {
  private val balance = MMap.empty[String, Balance]

  private var inFlight = 0
  private var processed = 0

  override protected def requestStrategy = new MaxInFlightRequestStrategy(10) {
    override def inFlightInternally = inFlight
  }

  def receive = {
    case OnNext(data: Transaction) =>
      processed += 1
      inFlight += 1
      updateBalance(data)
      inFlight -= 1

    case LogSummaryBalance => logger.info(s"(processed: $processed) - Balance so far: $balance")
  }

  def updateBalance(data: Transaction) = balance.get(data.accountNo).fold {
    balance += ((data.accountNo, Balance(data.amount, data.debitCredit)))
  } { b =>
    blocking {
      Thread.sleep(1)
      balance += ((data.accountNo, b |+| Balance(data.amount, data.debitCredit)))
    }
  }
}
