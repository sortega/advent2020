package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._
import scala.util.matching.Regex

object Day19 {

  enum Rule {
    case Literal(value: String)
    case Composed(alternatives: List[List[Int]])
  }

  case class Ruleset(rules: Map[Int, Rule]) {
    def toPattern(index: Int): String = rules(index) match {
      case Rule.Literal(value) => value
      case Rule.Composed(List(List(sub1), List(sub2, `index`))) if sub1 == sub2 =>
        toPattern(sub1)+"+"
      case Rule.Composed(List(List(before, after), List(before2, `index`, after2))) 
        if before == before2 && after == after2 =>
        s"${toPattern(before)}{%d}${toPattern(after)}{%d}"
      case Rule.Composed(alternatives) =>
        alternatives.map { sequence =>
          sequence.map(toPattern).mkString("(?:", "", ")")
        }.mkString("(?:", "|", ")")
    }
  }

  object Ruleset {
    def parse(lines: List[String]): Ruleset = Ruleset(lines.map {
      case s"""$id: "$text""""       => id.toInt -> Rule.Literal(text)
      case s"""$id: $references""" =>
        val alternatives = references.safeSplit("\\|")
          .map(_.safeSplit(" ").map(_.toInt))
        id.toInt -> Rule.Composed(alternatives)
    }.toMap)
  }

  case class Input(rules: Ruleset, messages: List[String])
  object Input {
    def parse(lines: List[String]): Input = {
      val (ruleLines, _ :: messages) = lines.span(_.trim.nn.nonEmpty)
      Input(Ruleset.parse(ruleLines), messages)
    }
  }

  def part1(input: Input): Int = {
    val patterns = {
      val pattern = input.rules.toPattern(0)
      if (pattern.contains("%d")) (1 to 10).map(n => pattern.replaceAll("%d", n.toString).nn.r)
      else List(pattern.r)
    }
    println(patterns)
    input.messages.count { message =>
      patterns.exists(_.matches(message))
    }
  }

  def part2(input: Input): Int = {
    val patchedInput = input.copy(rules = Ruleset(input.rules.rules ++ Map(
      8 -> Rule.Composed(List(List(42), List(42, 8))),
      11 -> Rule.Composed(List(List(42, 31), List(42, 11, 31)))
    )))
    part1(patchedInput)
  }

  def main(args: Array[String]): Unit = {
    val input = Input.parse(readInputLines(day = 19).toList)
    println(part1(input))
    println(part2(input))
  }
}
