package aoc

import aoc.Day23._
import org.junit.Assert._
import org.junit.Test

class Day23Test {
  
  @Test def part1Test(): Unit = {
    assertEquals("92658374", part1("389125467", moves = 10))
  }

  @Test def part2Test(): Unit = {
    assertEquals(149245887792L, part2("389125467"))
  }

}
