package aoc

import aoc.Day20._
import org.junit.Assert._
import org.junit.Test

class Day20Test {
  val input = Tile.parseAll(readInputLines(day = 20, test = true))
  val tile  = input.head
  
  @Test def orientationEncodingTest(): Unit = {
    assertEquals(0, Orientation(0, false))
    assertEquals(7, Orientation(3, true))
    assertEquals(Some((0, false)), Orientation.unapply(0))
    assertEquals(Some((3, true)), Orientation.unapply(7))
  }

  @Test def flipTest(): Unit = {
    assertEquals(
      """Tile 2311 (rot=0, flip=true):
        |Borders: Map(North -> 300, South -> 231, East -> 498, West -> 616)
        |.#..#.##..
        |.....#..##
        |.#..##...#
        |#...#.####
        |.###.##.##
        |###.#...##
        |##..#.#.#.
        |..#....#..
        |.#.#...###
        |###..###..
        |""".stripMargin,
      tile.horizontalFlip.toString
    )
  }

  @Test def rotateTest(): Unit = {
    assertEquals(
      """Tile 2311 (rot=1, flip=false):
        |Borders: Map(North -> 318, South -> 89, East -> 210, West -> 924)
        |.#..#####.
        |.#.####.#.
        |###...#..#
        |#..#.##..#
        |#....#.##.
        |...##.##.#
        |.#...#....
        |#.#.##....
        |##.###.#.#
        |#..##.#...
        |""".stripMargin,
      tile.rotate.toString
    )
    assertEquals(tile, tile.rotate.rotate.rotate.rotate)
  }

  @Test def orientationTest(): Unit = {
    assertEquals(
      """Tile 2311 (rot=2, flip=true):
        |Borders: Map(North -> 231, South -> 300, East -> 616, West -> 498)
        |..###..###
        |###...#.#.
        |..#....#..
        |.#.#.#..##
        |##...#.###
        |##.##.###.
        |####.#...#
        |#...##..#.
        |##..#.....
        |..##.#..#.
        |""".stripMargin,
      tile.withOrientation(6).toString
    )
    assertEquals(tile, tile.rotate.rotate.rotate.rotate)
  }


  @Test def borderTest(): Unit = {
    assertEquals(210, tile.borders(CardinalPoint.North))
    assertEquals(924, tile.borders(CardinalPoint.South))
    assertEquals(318, tile.borders(CardinalPoint.West))
    assertEquals(89, tile.borders(CardinalPoint.East))
  }

  @Test def part1Test(): Unit = {
    assertEquals(20899048083289L, part1(input))
  }
}
