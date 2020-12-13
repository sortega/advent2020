package aoc

import aoc.Day13._
import org.junit.Assert._
import org.junit.Test

class Day13Test {

  @Test def part1Test(): Unit = {
    assertEquals(295, part1(start = 939, buses = List(7, 13, 59, 31, 19)))
  }

  @Test def part2Test(): Unit = {
    assertEquals(3417L, part2(Bus.parse("17,x,13,19")))
    assertEquals(754018L, part2(Bus.parse("67,7,59,61")))
    assertEquals(779210L, part2(Bus.parse("67,x,7,59,61")))
    assertEquals(1261476L, part2(Bus.parse("67,7,x,59,61")))
    assertEquals(1202161486L, part2(Bus.parse("1789,37,47,1889")))
    assertEquals(1068781L, part2(Bus.parse("7,13,x,x,59,x,31,19")))
  }
}
