package aoc

import scala.annotation.tailrec

object Day09 {
  private def valid(addends: Set[Long], total: Long): Boolean = {
    val complementaries = addends.map(total - _)
    addends.intersect(complementaries).size >= 2
  }

  private def iterateWithPreamble(input: List[Long], size: Int): Iterator[(Set[Long], Long)] =
    input.iterator
      .sliding(size = size + 1, step = 1)
      .map { window =>
        val preamble = window.take(size).toSet
        val number   = window.last
        (preamble, number)
      }

  def part1(input: List[Long], preambleSize: Int = 25): Long =
    iterateWithPreamble(input, preambleSize).collectFirst {
      case (preamble, number) if !valid(preamble, number) => number
    }.get

  private def sublistsAddingTo(input: List[Long], target: Long) = {
    val cumulative    = input.scanLeft(0L)(_ + _)
    val complementary = cumulative.map(_ - target)
    val startingIndices = cumulative.toSet
      .intersect(complementary.toSet)
      .map(value => cumulative.indexOf(value))
    startingIndices.map { startIndex =>
      val startSum  = cumulative(startIndex)
      val targetSum = startSum + target
      val endIndex  = cumulative.indexOf(targetSum)
      input.slice(startIndex, endIndex)
    }
  }

  def part2(input: List[Long], target: Long): Long =
    sublistsAddingTo(input, target).collectFirst {
      case sublist if sublist.size > 1 => sublist.min + sublist.max
    }.get

  def main(args: Array[String]): Unit = {
    val input  = parseInputLines(day = 9)(_.toLong)
    val target = part1(input)
    println(target)
    println(part2(input, target))
  }
}
