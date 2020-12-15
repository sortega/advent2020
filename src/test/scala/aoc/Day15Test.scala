package aoc

import aoc.Day15._
import org.junit.Assert._
import org.junit.Test

class Day15Test {

  @Test def memoryGameTest(): Unit = {
    assertEquals(
      LazyList(0, 3, 6, 0, 3, 3, 1, 0, 4, 0),
      memoryGameSequence(List(0, 3, 6)).take(10)
    )
  }

  @Test def part1Test(): Unit = {
    assertEquals(436, part1(List(0, 3, 6)))
  }
}
