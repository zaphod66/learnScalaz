package com.myspace.morsecode

// The Monad Reader Issue 14 'Fun with Morse codes'

object Morse {
  type MorseCode = String
  val dedict = Map[MorseCode, Char](
    "-."    -> 'A',
    "-..."  -> 'B',
    "-.-."  -> 'C',
    "-.."   -> 'D',
    "."     -> 'E',
    "..-."  -> 'F',
    "--."   -> 'G',
    "...."  -> 'H',
    ".."    -> 'I',
    ".---"  -> 'J',
    "-.-"   -> 'K',
    ".-.."  -> 'L',
    "--"    -> 'M',
    "-."    -> 'N',
    "---"   -> 'O',
    ".--."  -> 'P',
    "--.-"  -> 'Q',
    ".-."   -> 'R',
    "..."   -> 'S',
    "-"     -> 'T',
    "..-"   -> 'U',
    "...-"  -> 'V',
    ".--"   -> 'W',
    "-..-"  -> 'X',
    "-.--"  -> 'Y',
    "--.."  -> 'Z',
    "-----" -> '0',
    ".----" -> '1',
    "..---" -> '2',
    "...--" -> '3',
    "....-" -> '4',
    "....." -> '5',
    "-...." -> '6',
    "--..." -> '7',
    "---.." -> '8',
    "----." -> '9'
  )

  val endict = dedict map { _.swap }
}
object MorseTest extends App {
  println("MorseTest")

  import Morse._

  def decodeLetter(c: MorseCode): Option[Char] = dedict.get(c)

  def decode(s: String): String = {
    val t = s.split(" ") map decodeLetter

    t.toList.flatten.mkString
  }

  def encodeLetter(c: Char): Option[MorseCode] = endict.get(c)

  def encode(s: String): String = {
    val t = s.toList map encodeLetter

    t.flatten.mkString(" ")
  }

  val code = "-- --- .-. ... . -.-. --- -.. ."
  val text = decode(code)

  println(s"$code => $text")
  println(s"$text => ${encode(text)}")

}
