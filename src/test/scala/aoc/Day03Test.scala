package aoc

import aoc.Day03
import aoc.Day03.Area
import org.junit.Assert._
import org.junit.Test

class Day03Test {
  val input = Area.parse(readInputLines(day = 3, test = true))

  @Test def part1Test(): Unit = {
    assertEquals(BigDecimal(7), Day03.part1(input))
  }

  @Test def part2Test(): Unit = {
    assertEquals(BigDecimal(2), Day03.countTrees(input, Pos(1, 1)))
    assertEquals(BigDecimal(7), Day03.countTrees(input, Pos(1, 3)))
    assertEquals(BigDecimal(3), Day03.countTrees(input, Pos(1, 5)))
    assertEquals(BigDecimal(4), Day03.countTrees(input, Pos(1, 7)))
    assertEquals(BigDecimal(2), Day03.countTrees(input, Pos(2, 1)))
    assertEquals(BigDecimal(336), Day03.part2(input))
  }
}
