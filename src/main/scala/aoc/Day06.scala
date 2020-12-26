package aoc

object Day06 {
  case class Group(passengers: List[Set[Char]]) {
    def uniqueAnswers: Set[Char] = passengers.reduce(_.union(_))
    def commonAnswers: Set[Char] = passengers.reduce(_.intersect(_))
  }

  object Group {
    def parse(lines: List[String]): Group = Group(lines.map(_.trim.nn.toSet))
  }
  
  def part1(input: List[Group]): Int = input.map(_.uniqueAnswers.size).sum

  def part2(input: List[Group]): Int = input.map(_.commonAnswers.size).sum

  def main(args: Array[String]): Unit = {
    val input = parseInputGroupedLines(day = 6)(Group.parse)
    println(part1(input))
    println(part2(input))
  }
}
