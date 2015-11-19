package com.myspace.stuff

import java.io.{File, PrintStream}

object ScalaWordCount {
  def _main(args: Array[String]) = {

    if (args.size < 2) {
      println("usage: wordCount <in.txt> <out.txt>")
    } else {
      val lines = scala.io.Source.fromFile(args(0)).getLines()

      val wordsCounted = lines.map(line => line.toLowerCase)
        .flatMap(line => line.split("""\W+""")).toSeq
        .groupBy(word => word)
        .map { case (word, group) => (word, group.size) }

      val wcList = wordsCounted.toList.sortBy(_._2).reverse

      val out = new PrintStream(new File(args(1)))

      wcList foreach {
        wordCount => out.println(wordCount)
      }
    }
  }
}
