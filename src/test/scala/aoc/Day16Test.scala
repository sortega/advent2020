package aoc

import aoc.Day16._
import org.junit.Assert._
import org.junit.Test

class Day16Test {
  val input = Input.parse(readInputLines(day = 16, test = true).toList)

  @Test def part1Test(): Unit = {
    assertEquals(71, part1(input))
  }

  @Test def part2Test(): Unit = {
    val input = Input.parse(
      """class: 0-1 or 4-19
        |row: 0-5 or 8-19
        |seat: 0-13 or 16-19
        |
        |your ticket:
        |11,12,13
        |
        |nearby tickets:
        |3,9,18
        |15,1,5
        |5,14,9
        |""".stripMargin.linesIterator.toList)
    assertEquals(Map("class" -> 1, "row" -> 0, "seat" -> 2), input.fieldPositions)
    assertEquals(BigInt(1), part2(input))
  }
}
