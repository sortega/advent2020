package aoc

import aoc.Day12._
import org.junit.Assert._
import org.junit.Test

class Day12Test {

  private val sampleCommands =
    """F10
      |N3
      |F7
      |R90
      |F11
      |""".stripMargin.linesIterator.map(Command.parse).toList

  @Test def part1Test(): Unit = {
    assertEquals(25, part1(sampleCommands))
  }

  @Test def part2Test(): Unit = {
    assertEquals(286, part2(sampleCommands))
  }
}
