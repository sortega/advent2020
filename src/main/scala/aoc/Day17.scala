package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._

object Day17 {
  case class Point(values: List[Int]) extends AnyVal {
    def +(other: Point): Point = Point(values.zip(other.values).map(_ + _))

    def neighbors: Set[Point] = values
      .foldLeft(Set(List.empty[Int])) { (prefixes, value) =>
        (for {
          delta <- -1 to 1
          prefixValue = delta + value
          prefix <- prefixes
        } yield prefixValue :: prefix).toSet
      }
      .map(prefix => Point(prefix.reverse)) - this
  }

  case class World(active: Set[Point]) extends AnyVal {
    def step: World = {
      val neighborCount: Map[Point, Int] = active.toList
        .flatMap(_.neighbors)
        .groupMapReduce(identity)(_ => 1)(_ + _)
      World(neighborCount.collect {
        case (cell, 3) if !active(cell)    => cell
        case (cell, 2 | 3) if active(cell) => cell
      }.toSet)
    }
  }

  object World {
    def parse(lines: List[String], dimension: Int): World = World(
      (for {
        (line, y) <- lines.zipWithIndex
        (char, x) <- line.zipWithIndex
        if char == '#'
      } yield Point(List(x, y).padTo(dimension, 0))).toSet
    )
  }

  def part(input: World): Int =
    LazyList.iterate(input)(_.step).drop(6).head.active.size

  def main(args: Array[String]): Unit = {
    val input = readInputLines(day = 17).toList
    println(part(World.parse(input, dimension = 3)))
    println(part(World.parse(input, dimension = 4)))
  }
}
