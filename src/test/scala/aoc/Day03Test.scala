package aoc

import aoc.Day03
import aoc.Day03.{Area, Pos}
import org.junit.Assert._
import org.junit.Test

class Day03Test {
  val input = Area.parse("""..##.......
                           |#...#...#..
                           |.#....#..#.
                           |..#.#...#.#
                           |.#...##..#.
                           |..#.##.....
                           |.#.#.#....#
                           |.#........#
                           |#.##...#...
                           |#...##....#
                           |.#..#...#.#
                           |""".stripMargin.linesIterator)

  @Test def part1Test(): Unit = {
    assertEquals(7, Day03.part1(input))
  }

  @Test def part2Test(): Unit = {
    assertEquals(2, Day03.countTrees(input, Pos(1, 1)))
    assertEquals(7, Day03.countTrees(input, Pos(1, 3)))
    assertEquals(3, Day03.countTrees(input, Pos(1, 5)))
    assertEquals(4, Day03.countTrees(input, Pos(1, 7)))
    assertEquals(2, Day03.countTrees(input, Pos(2, 1)))
    assertEquals(336, Day03.part2(input))
  }
}
