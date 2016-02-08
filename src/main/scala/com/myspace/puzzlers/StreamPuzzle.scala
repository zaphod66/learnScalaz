package com.myspace.puzzlers

object StreamPuzzle extends App {

  val nats: Stream[Int] = 1 #:: (nats map { _ + 1 })
  val odds: Stream[Int] = 1 #:: (odds map { _ + 1 } filter { _ % 2 != 0 })

//nats filter { _ % 2 != 0 } take 2 foreach println
  odds take 2 foreach println
}
