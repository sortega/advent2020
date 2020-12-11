package aoc

import aoc.Day11._
import org.junit.Assert._
import org.junit.Test

class Day11Test {

  private val initialSeats = WaitingRoom.parse("""#.##.##.##
                                                 |#######.##
                                                 |#.#.#..#..
                                                 |####.##.##
                                                 |#.##.##.##
                                                 |#.#####.##
                                                 |..#.#.....
                                                 |##########
                                                 |#.######.#
                                                 |#.#####.##
                                                 |""".stripMargin)

  @Test def parseTest(): Unit = {
    val input  = """#..#
                  |#LL.
                  |""".stripMargin
    val parsed = WaitingRoom.parse(input)
    val expected = WaitingRoom(
      seating = Seating(Set(Pos(0, 0), Pos(0, 3), Pos(1, 0), Pos(1, 1), Pos(1, 2))),
      people = Set(Pos(0, 0), Pos(0, 3), Pos(1, 0))
    )
    assertEquals(expected, parsed)
    assertEquals(input, parsed.toString)
  }

  @Test def part1Test(): Unit = {
    assertEquals(37, part1(initialSeats))
  }

  @Test def part2Test(): Unit = {
    assertEquals(26, part2(initialSeats))
  }
}
