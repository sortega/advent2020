package aoc

object Day01 {

  def part1(entries: List[Int]): Int = {
    val set = entries.toSet
    val complementarySet = set.map(2020 - _)
    set.intersect(complementarySet).product
  }

  def part2(entries: List[Int]): Int =
    (for {
      first <- entries.to(LazyList)
      available = entries.toSet - first
      second <- available
      third = 2020 - first - second
      if available.contains(third)
    } yield first * second * third).head

  def main(args: Array[String]): Unit = {
    val input = parseInputLines(day = 1)(_.toInt)
    println(part1(input))
    println(part2(input))
  }
}
