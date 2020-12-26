package aoc

import aoc.Day22._
import org.junit.Assert._
import org.junit.Test

class Day22Test {
  val input = Game.parse(
    """Player 1:
      |9
      |2
      |6
      |3
      |1
      |
      |Player 2:
      |5
      |8
      |4
      |7
      |10
      |""".stripMargin.linesIterator)
  
  @Test def part1Test(): Unit = {
    assertEquals(306, part1(input))
  }

  @Test def part2Test(): Unit = {
    assertEquals(291, part2(input))
  }
}
