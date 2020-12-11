package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 {
  case class Seating(seats: Set[Pos]) {
    val maxRow = seats.map(_.r).max
    val maxCol = seats.map(_.c).max

    val weirdAdjancency: Map[Pos, Set[Pos]] = seats.map(s => s -> adjacentTo(s)).toMap

    private def adjacentTo(seat: Pos): Set[Pos] =
      Pos.Adjacent.flatMap(dir => adjacentInDirection(seat, dir))

    private def adjacentInDirection(seat: Pos, dir: Pos): Option[Pos] =
      LazyList
        .iterate(seat + dir)(_ + dir)
        .takeWhile(
          seat => seat.r >= 0 && seat.c >= 0 && seat.r <= maxRow && seat.c <= maxCol
        )
        .filter(seats.contains)
        .headOption
  }

  case class WaitingRoom(seating: Seating, people: Set[Pos]) {
    def step(adjacentTo: Pos => Set[Pos], maxNeighbors: Int): WaitingRoom =
      copy(people = seating.seats.filter { seat =>
        val neighbors     = adjacentTo(seat).intersect(people).size
        val occupied      = people.contains(seat)
        val newPerson     = !occupied && neighbors == 0
        val stayingPerson = occupied && neighbors < maxNeighbors
        newPerson || stayingPerson
      })

    override def toString: String =
      (0 to seating.maxRow)
        .map { r =>
          (0 to seating.maxCol).map { c =>
            val pos = Pos(r, c)
            (seating.seats(pos), people(pos)) match {
              case (_, true) => '#'
              case (true, _) => 'L'
              case _         => '.'
            }
          }.mkString
        }
        .mkString("", "\n", "\n")
  }

  object WaitingRoom {
    def parse(lines: String): WaitingRoom = {
      val chars = (for {
        (line, row) <- lines.linesIterator.zipWithIndex
        (char, col) <- line.zipWithIndex
      } yield Pos(row, col) -> char).toMap
      val seats = chars.collect {
        case (pos, 'L' | '#') => pos
      }.toSet
      val people = chars.collect {
        case (pos, '#') => pos
      }.toSet
      WaitingRoom(Seating(seats), people)
    }
  }

  @tailrec def findStableState[A](state: A)(step: A => A): A = {
    val nextState = step(state)
    if (nextState == state) state
    else findStableState(nextState)(step)
  }

  def part1(input: WaitingRoom): Int =
    findStableState(input)(_.step(_.adjacent, maxNeighbors = 4)).people.size

  def part2(input: WaitingRoom): Int =
    findStableState(input)(_.step(input.seating.weirdAdjancency, maxNeighbors = 5)).people.size

  def main(args: Array[String]): Unit = {
    val input = WaitingRoom.parse(readInputLines(day = 11).mkString("\n"))
    println(part1(input))
    println(part2(input))
  }

}
