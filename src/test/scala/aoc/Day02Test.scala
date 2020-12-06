package aoc

import Day02._
import org.junit.Assert._
import org.junit.Test

class Day02Test {
  val input = parseInputLines(day = 2, test = true)(parseEntry)

  @Test def part1Test(): Unit = {
    assertEquals(2, Day02.part1(input))
  }

  @Test def part2Test(): Unit = {
    assertEquals(1, Day02.part2(input))
  }
}