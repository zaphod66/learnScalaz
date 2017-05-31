package com.myspace.streams

import java.nio.file.{Paths, Files}
import java.nio.charset._

import scala.util.Random
import scala.collection.JavaConverters._

object Generator {

  val accounts = Vector("a-123", "a-234", "a-345", "a-456")
  val amounts = Vector(100, 200, 300, 400, 500).map(BigDecimal(_))
  val dc = Vector("d", "c")

  val records: Seq[java.lang.String] = ( 1 to 50000 ).map { i =>
    s"$i,${accounts(Random.nextInt(4))},${dc(Random.nextInt(2))},${amounts(Random.nextInt(5))}"
  }

  def generate() = {
    val path = "/Users/nscheller/work/gitrepos/projects/zaphod66/learnScalaz/src/main/resources/transactions.csv"
    val p = Files.write(Paths.get(path), records.asJava, Charset.defaultCharset)
    println(s"** p = $p")
  }

}
