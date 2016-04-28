package com.myspace.learning

import scalaz._, Scalaz._
import effect._, IO._

object day17 extends App {

  val action1 = for {
    _ <- putStrLn("Hello, world!")
  } yield ()

  action1.unsafePerformIO
}
