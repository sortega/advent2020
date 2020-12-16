package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._

object Day16 {
  type Ticket = List[Int]
  object Ticket {
    def parse(input: String): Ticket = input.split(",").map(_.toInt).toList
  }

  case class FieldConstraint(name: String, ranges: List[Range]) {
    def apply(value: Int): Boolean = ranges.exists(_.contains(value))
  }

  object FieldConstraint {
    def parse(input: String): FieldConstraint = input match {
      case s"$name: $from1-$to1 or $from2-$to2" =>
        FieldConstraint(
          name,
          List(
            from1.toInt to to1.toInt,
            from2.toInt to to2.toInt
          )
        )
    }
  }

  case class Input(
      fieldConstraints: Map[String, FieldConstraint],
      yourTicket: Ticket,
      tickets: List[Ticket]) {
    def fieldPositions: Map[String, Int] =
      validTickets.transpose
        .map(_.map(feasibleFor).reduce(_.intersect(_)))
        .pipe(constrain)
        .zipWithIndex
        .toMap

    private def constrain(possibilities: List[Set[String]]): List[String] = {
      def go(state: List[Either[String, Set[String]]]): List[String] =
        if (state.forall(_.isLeft)) state.map(_.left.get)
        else {
          val index = state.indexWhere {
            case Right(singleOption) if singleOption.size == 1 => true
            case _                                             => false
          }
          require(index >= 0, "assuing we don't need backtracking")
          val value = state(index).right.get.head
          go(state.map(_.map(_ - value)).updated(index, Left(value)))
        }
      go(possibilities.map(Right.apply))
    }

    private def validTickets: List[Ticket] = tickets.filter { ticket =>
      ticket.forall(value => fieldConstraints.values.exists(c => c(value)))
    }

    private def feasibleFor(value: Int): Set[String] =
      fieldConstraints.values.collect {
        case cons if cons(value) => cons.name
      }.toSet
  }

  object Input {
    def parse(input: List[String]): Input = {
      val emptyLines = input.zipWithIndex.collect { case ("", index) =>
        index
      }
      Input(
        input
          .take(emptyLines(0))
          .map { line =>
            val c = FieldConstraint.parse(line)
            c.name -> c
          }
          .toMap,
        Ticket.parse(input(emptyLines(0) + 2)),
        input.drop(emptyLines(1) + 2).map(Ticket.parse)
      )
    }
  }

  def part1(input: Input): Int =
    input.tickets.flatten.filter { value =>
      input.fieldConstraints.values.forall(c => !c(value))
    }.sum

  def part2(input: Input): BigInt = input.fieldPositions.collect {
    case (name, index) if name.startsWith("departure") => BigInt(input.yourTicket(index))
  }.product

  def main(args: Array[String]): Unit = {
    val input = Input.parse(readInputLines(day = 16).toList)
    println(part1(input))
    println(part2(input))
  }
}
