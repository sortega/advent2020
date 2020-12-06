package aoc

import aoc.Day06._
import org.junit.Assert._
import org.junit.Test

class Day06Test {
  val input = parseInputGroupedLines(day = 6, test = true)(Group.parse)

  @Test def part1Test(): Unit = {
    assertEquals(11, part1(input))
  }

  @Test def part2Test(): Unit = {
    assertEquals(6, part2(input))
  }
}
