package aoc

import org.junit.Assert._
import org.junit.Test

class Day02Test {
  val input =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc
      |""".stripMargin.linesIterator.map(Day02.parseEntry).toList

  @Test def part1Test(): Unit = {
    assertEquals(2, Day02.part1(input))
  }

  @Test def part2Test(): Unit = {
    assertEquals(1, Day02.part2(input))
  }
}