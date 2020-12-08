package aoc

import scala.annotation.tailrec

object Day07 {
  type Color = Symbol
  val ShinyGold = Symbol("shiny gold")
  type Chain = List[Rule]

  case class Rule(bagColor: Color, contents: Map[Color, Int])

  object Rule {
    private val RulePattern     = """(\w+ \w+) bags contain ([^.]+)\.""".r
    private val ContentsPattern = """\s*(\d+) (\w+ \w+) bags?""".r

    def parse(input: String): Rule = input match {
      case RulePattern(bagColor, contents) => Rule(Symbol(bagColor), parseContents(contents))
    }

    private def parseContents(contents: String): Map[Color, Int] =
      if (contents == "no other bags") Map.empty
      else
        contents
          .split(",")
          .map {
            case ContentsPattern(amount, color) => (Symbol(color), amount.toInt)
          }
          .toMap
  }

  private def chainsContaining(rules: List[Rule], content: Color): Set[Chain] = {
    val smallestChains: Set[Chain] = rules.filter(_.contents.contains(content)).map(List(_)).toSet
    closeOver(smallestChains) { chain =>
      rules.collect {
        case rule if rule.contents.contains(chain.head.bagColor) => rule :: chain
      }.toSet
    }
  }
  
  private def closeOver[A](seed: Set[A])(step: A => Set[A]): Set[A] = {
    val sequence = LazyList.iterate(seed)(elems => elems.map(step).foldLeft(elems)(_.union(_)))
    sequence.zip(sequence.tail).collectFirst {
      case (prev, next) if prev == next => prev
    }.getOrElse(throw new IllegalStateException("cannot reach here"))
  }

  def part1(input: List[Rule]): Int =
    chainsContaining(input, ShinyGold).map(_.head.bagColor).size

  def part2(input: List[Rule]): Int = {
    val ruleFor = input.map(rule => rule.bagColor -> rule.contents).toMap
    
    @tailrec
    def go(bags: Map[Color, Int], total: Int = 0): Int = {
      if (bags.isEmpty) total
      else {
        val openedBags = bags.values.sum
        val containedBags = toMergedMap(for {
          (color, amount) <- bags.toList
          (contentColor, contentAmount) <- ruleFor(color)
        } yield (contentColor, amount * contentAmount))
        go(containedBags, total + openedBags)
      }
    }
    
    go(Map(ShinyGold -> 1)) - 1
  }
  
  private def toMergedMap(tuples: List[(Color, Int)]): Map[Color, Int] =
    tuples.groupMapReduce(_._1)(_._2)(_ + _)

  def main(args: Array[String]): Unit = {
    val input = parseInputLines(day = 7)(Rule.parse)
    println(part1(input))
    println(part2(input))
  }
}
