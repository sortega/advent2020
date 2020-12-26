package aoc

object Day02 {
  case class Policy(char: Char, min: Int, max: Int) {
    def matchesOldPolicy(password: String): Boolean = {
      val count = password.count(_ == char)
      count >= min && count <= max
    }

    def matchesNewPolicy(password: String): Boolean = {
      val selectedChars = Set(password(min - 1), password(max - 1))
      selectedChars.size > 1 && selectedChars.contains(char)
    }
  }

  case class Entry(policy: Policy, password: String) {
    def matchesOldPolicy: Boolean = policy.matchesOldPolicy(password)
    def matchesNewPolicy: Boolean = policy.matchesNewPolicy(password)
  }

  def parseEntry(input: String): Entry = {
    val pattern = raw"(\d+)-(\d+) (\w): (\w+)".r
    input match {
      case pattern(min, max, char, password) =>
        Entry(Policy(char.head, min.toInt, max.toInt), password)
    }
  }

  def part1(entries: List[Entry]): Int = entries.count(_.matchesOldPolicy)
  def part2(entries: List[Entry]): Int = entries.count(_.matchesNewPolicy)

  def main(args: Array[String]): Unit = {
    val input = parseInputLines(day = 2)(parseEntry)
    println(part1(input))
    println(part2(input))
  }
}
