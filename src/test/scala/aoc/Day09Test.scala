package aoc

import aoc.Day09._
import org.junit.Assert._
import org.junit.Test

class Day09Test {

  private val input =
    List[Long](35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)

  @Test def part1Test(): Unit = {
    assertEquals(127, part1(input, preambleSize = 5))
  }

  @Test def part2Test(): Unit = {
    assertEquals(62, part2(input, target = 127))
  }
}
