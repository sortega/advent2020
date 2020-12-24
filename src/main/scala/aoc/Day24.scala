package aoc

import scala.util.chaining._

object Day24 {

  case class Path(steps: List[HexDir]) {
    def toPos: Pos = steps.map(_.vector).foldLeft(Pos.Origin)(_ + _)
  }

  object Path {
    def parse(input: String): Path =
      if (input.trim.isEmpty) Path(Nil)
      else
        HexDir.values.collectFirst {
          case dir if input.startsWith(dir.toString) =>
            Path(dir :: parse(input.drop(dir.toString.length)).steps)
        }.get
  }

  def toActiveTiles(input: List[Path]): Set[Pos] = input
    .groupMapReduce(_.toPos)(_ => 1)(_ + _)
    .collect {
      case (pos, oddFlips) if oddFlips % 2 == 1 => pos
    }
    .toSet

  def part1(input: List[Path]): Int = toActiveTiles(input).size

  case class World(active: Set[Pos]) extends AnyVal {
    def step: World = {
      val neighborCount: Map[Pos, Int] = active.toList
        .flatMap(_.hexAdjacent)
        .groupMapReduce(identity)(_ => 1)(_ + _)
      World(neighborCount.collect {
        case (cell, 2) if !active(cell)    => cell
        case (cell, 1 | 2) if active(cell) => cell
      }.toSet)
    }
  }

  def part2(input: List[Path], steps: Int = 100): Int = 
    Iteration.repeatedly(World(toActiveTiles(input)), steps)(_.step).active.size

  def main(args: Array[String]): Unit = {
    val input = parseInputLines(day = 24)(Path.parse)
    println(part1(input))
    println(part2(input))
  }
}
