package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._
import scala.util.matching.Regex

object Day16 {
  type Ticket = List[Int]

  case class FieldConstraint(name: String, ranges: List[Range]) {
    def apply(value: Int): Boolean = ranges.exists(_.contains(value))
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
    import scala.util.parsing.combinator._

    object Grammar extends RegexParsers {
      override protected val whiteSpace: Regex = "[ ]+".r

      def number: Parser[Int]  = """\d+""".r ^^ { _.toInt }
      def range: Parser[Range] = (number ~ "-" ~ number) ^^ { case a ~ _ ~ b => a to b }
      def ticket: Parser[Ticket] = (number ~ ("," ~> number).*) ^^ { _ :: _}
      def fieldName: Parser[String] = ("""[a-z ]+""".r <~ ":") ^^ { _.toString }
      def fieldConstraint: Parser[FieldConstraint] =
        (fieldName ~ range ~ ("or" ~> range)) ^^ {
          case name ~ range1 ~ range2 => FieldConstraint(name, List(range1, range2))
        }
      def fieldConstraints: Parser[Map[String, FieldConstraint]] =
        (fieldConstraint <~ "\n").+ ^^ { constraints =>
          constraints.map(c => c.name -> c).toMap
        }
        
      def yourTicket: Parser[Ticket] = "your ticket:" ~ "\n" ~> ticket <~ "\n"
      
      def nearbyTickets: Parser[List[Ticket]] = 
        "nearby tickets:" ~ "\n" ~> (ticket <~ "\n").+
        
      def input: Parser[Day16.Input] =
        ((fieldConstraints <~ "\n") ~ (yourTicket <~ "\n") ~ nearbyTickets) ^^ {
          case constraints ~ ticket ~ tickets => Day16.Input(constraints, ticket, tickets)
        }
    }

    def parse(input: List[String]): Input = 
      Grammar.parse(Grammar.input, input.mkString("", "\n", "\n")).get
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
