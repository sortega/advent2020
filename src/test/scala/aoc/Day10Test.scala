package aoc

import aoc.Day10._
import org.junit.Assert._
import org.junit.Test

class Day10Test {
  val shortInput = List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
  val longInput = List(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1,
    32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)

  @Test def part1Test(): Unit = {
    assertEquals(35, part1(shortInput))
    assertEquals(220, part1(longInput))
  }

  @Test def part2Test(): Unit = {
    assertEquals(8L, part2(shortInput))
    assertEquals(19208L, part2(longInput))
  }
}
