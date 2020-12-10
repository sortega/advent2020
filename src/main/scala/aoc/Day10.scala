package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day10 {

  def part1(input: List[Int]): Int = {
    val adapters    = input.sorted
    val deviceJolts = adapters.last + 3
    val levels      = 0 :: (adapters :+ deviceJolts)
    val jumps = levels.iterator
      .sliding(size = 2, step = 1)
      .map {
        case Seq(prev, next) => next - prev
      }
      .toList
    val distro = jumps.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)
    distro(1) * distro(3)
  }

  def part2(input: List[Int]): Long = {
    val adapters    = input.sorted
    val deviceJolts = adapters.last + 3
    val countArrangements = Memo.recursive[Int, List[Int], Long] { (from, adapters, recur) =>
      adapters match {
        case Nil                                => if (deviceJolts - from <= 3) 1L else 0L
        case adapter :: _ if adapter > from + 3 => 0L
        case adapter :: rest                    => recur(adapter, rest) + recur(from, rest)
      }
    }
    countArrangements(0, adapters)
  }

  def main(args: Array[String]): Unit = {
    val input = parseInputLines(day = 10)(_.toInt)
    println(part1(input))
    println(part2(input))
  }

}
