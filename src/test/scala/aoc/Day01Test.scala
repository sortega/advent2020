package aoc

import org.junit.Test
import org.junit.Assert._

class Day01Test {
  val input = List(1721, 979, 366, 299, 675, 1456)

  @Test def part1Test(): Unit = {
    assertEquals(514579, Day01.part1(input))
  }

  @Test def part2Test(): Unit = {
    assertEquals(241861950, Day01.part2(input))
  }
}
