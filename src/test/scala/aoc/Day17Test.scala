package aoc

import aoc.Day17._
import org.junit.Assert._
import org.junit.Test

class Day17Test {
  val input = readInputLines(day = 17, test = true).toList

  @Test def pointTest(): Unit = {
    assertEquals(
      Set(
        // format: off
        Point(List(-1, -1)), Point(List( 0, -1)), Point(List( 1, -1)),
        Point(List(-1,  0)),                      Point(List( 1,  0)),
        Point(List(-1,  1)), Point(List( 0,  1)), Point(List( 1,  1)),
        // format: on
      ),
      Point(List(0, 0)).neighbors
    )
    assertEquals(26, Point(List(0, 0, 0)).neighbors.size)
    assertEquals(80, Point(List(0, 0, 0, 0)).neighbors.size)
  }

  @Test def part1Test(): Unit = {
    assertEquals(112, part(World.parse(input, dimension = 3)))
  }

  @Test def part2Test(): Unit = {
    assertEquals(848, part(World.parse(input, dimension = 4)))
  }
}
