package com.myspace.uri

import scala.io.Source
import scala.concurrent.duration._
import scala.language.postfixOps

import rx.lang.scala.{Observer, Observable}

import java.net.{URL, URI}


object UriTest extends App {

  // > run file://localhost/Users/nscheller/Downloads/Archive_5.zip

  if (args.size != 1) {
    println(s"run <uri>")
    sys.exit(-1)
  }

  val baseUriStr = args(0)
  val baseUri = URI.create(baseUriStr)

  val uriPath = baseUri.getPath

  val urlStr = s"jar:file://$uriPath!/"

  val txnUrl = new URL(s"${urlStr}transactions.csv")
  val devUrl = new URL(s"${urlStr}devices.csv")
  val itmUrl = new URL(s"${urlStr}items.csv")

  val txnLines = Source.fromURL(txnUrl).getLines()
  val devLines = Source.fromURL(devUrl).getLines().zipWithIndex
  val itmLines = Source.fromURL(itmUrl).getLines().zipWithIndex

  val fileObservable = Observable.just("transactions.csv", "devices.csv")

  val txnObservable = Observable.from(txnLines.toIterable)
  val txnObserver = Observer[String](
    (line: String) => println(line),
    (e: Throwable) => println(e.getMessage),
    () => println("Done!")
  )

  val itmObservable = Observable.from(itmLines.toIterable)
  val itmObserver = Observer[(String,Int)](
    (ln: (String,Int)) => println(s"${ln._2}: ${ln._1}"),
    () => println("Done!")
  )

  println("===========")
  val txnSubscription = txnObservable subscribe txnObserver
  txnSubscription.unsubscribe()
  println("===========")
  val itmSubscription = itmObservable subscribe itmObserver
  itmSubscription.unsubscribe()
  println("===========")
  devLines foreach { println }
  println("===========")
}
