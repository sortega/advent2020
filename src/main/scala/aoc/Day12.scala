package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._
import aoc.CardinalPoint._
import aoc.Side._

object Day12 {
  enum Command(val code: Char) {
    case MoveForward(units: Int) extends Command('F')
    case Move(towards: CardinalPoint, units: Int) extends Command(towards.toString.head)
    case Rotate(side: Side, units: Int) extends Command(side.toString.head)

    def units: Int
    override def toString: String = s"$code$units"
  }

  object Command {
    private val Pattern = """(.)(\d+)""".r
    def parse(input: String): Command = {
      val Pattern(code, stringUnits) = input
      val units = stringUnits.toInt
      code match {
        case "F" => MoveForward(units)
        case "N" => Move(North, units)
        case "E" => Move(East, units)
        case "S" => Move(South, units)
        case "W" => Move(West, units)
        case "L" => Rotate(Left, units)
        case "R" => Rotate(Right, units)
      }
    }
  }

  import Command._

  case class Ship(pos: Pos = Pos.Origin, facing: CardinalPoint = East) {
    def run(command: Command): Ship = command match {
      case MoveForward(units) => copy(pos = pos + facing.vector * units)
      case Move(towards, units) => copy(pos = pos + towards.vector * units)
      case Rotate(side, degrees) =>
        copy(facing = facing + CardinalPoint.fromDegrees(-side.sign * degrees))
    }
  }

  case class WaypointShip(pos: Pos = Pos.Origin, waypoint: Pos = Pos(-1, 10)) {
    def run(command: Command): WaypointShip = command match {
      case MoveForward(units) => copy(pos = pos + waypoint * units)
      case Move(towards, units) => copy(waypoint = waypoint + towards.vector * units)
      case Rotate(side, degrees) =>
        copy(waypoint = waypoint * CardinalPoint.fromDegrees(-side.sign * degrees).vector)
    }
  }

  def part1(input: List[Command]): Int = input.foldLeft(Ship())(_.run(_)).pos.manhattanNorm
  def part2(input: List[Command]): Int = input.foldLeft(WaypointShip())(_.run(_)).pos.manhattanNorm

  def main(args: Array[String]): Unit = {
    val input = parseInputLines(day = 12)(Command.parse)
    println(part1(input))
    println(part2(input))
  }

}
