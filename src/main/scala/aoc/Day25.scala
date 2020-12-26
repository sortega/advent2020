package aoc

import scala.util.chaining._

import aoc.Iteration.repeatedly

object Day25 {
  val P = 20201227

  def transform(subject: Long, size: Int): Long = repeatedly(1L, size)(_ * subject % P)

  val transformations7: LazyList[Long] = LazyList.iterate(1L)(_ * 7 % P)

  def sizeFor(value: Long): Int = transformations7.indexOf(value)

  def part1(cardPk: Long, doorPk: Long): Long =
    val cardSize = sizeFor(cardPk)
    transform(doorPk, cardSize)

  def main(args: Array[String]): Unit =
    println(part1(cardPk = 17773298, doorPk = 15530095))
}
