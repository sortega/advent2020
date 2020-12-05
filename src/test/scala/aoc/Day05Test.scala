package aoc

import aoc.Day05._
import org.junit.Assert._
import org.junit.Test

class Day05Test {
  @Test def part1Test(): Unit = {
    assertEquals(BoardingPass(row = 70, col = 7), pass"BFFFBBFRRR")
    assertEquals(567, pass"BFFFBBFRRR".seatId)
  }

  @Test def part2Test(): Unit = {
    assertEquals(List(4, 6), findHoles(List(2, 3, 5, 7)).toList)
  }
}
