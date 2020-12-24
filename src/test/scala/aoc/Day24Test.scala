package aoc

import aoc.Day24._
import org.junit.Assert._
import org.junit.Test

class Day24Test {
  val input = parseInputLines(day = 24, test = true)(Path.parse)

  @Test def part1Test(): Unit = {
    assertEquals(10, part1(input))
  }

  @Test def part2Test(): Unit = {
    assertEquals(15, part2(input, steps = 1))
    assertEquals(12, part2(input, steps = 2))
    assertEquals(25, part2(input, steps = 3))
    assertEquals(2208, part2(input))
  }

}
