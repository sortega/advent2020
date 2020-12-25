package aoc

import scala.util.chaining._

import aoc.Iteration.repeatedly

object Day25 {
  val P = 20201227
  
  def transform(subject: Long, size: Int): Long = repeatedly(1L, size)(_ * subject % P)
  
  val transformations7: LazyList[Long] = LazyList.iterate(1L)(_ * 7 % P)
  
  def sizeFor(value: Long): Int = transformations7.indexOf(value)

  def part1(cardPk: Long, doorPk: Long): Long = {
    val cardSize = sizeFor(cardPk)
    transform(doorPk, cardSize)
  }
  
  def part2(cardPk: Long, doorPk: Long): Int = ???

  def main(args: Array[String]): Unit = {
    val cardPk = 17773298
    val doorPk = 15530095
    println(part1(cardPk, doorPk))
    println(part2(cardPk, doorPk))
  }
}
