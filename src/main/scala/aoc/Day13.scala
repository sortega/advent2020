package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._

import aoc.ModularArithmetic._

object Day13 {

  def waitTime(start: Int, freq: Int): Int = (freq - start) mod freq

  def part1(start: Int, buses: List[Int]): Int = {
    val (bus, time) = buses
      .map { freq =>
        (freq, waitTime(start, freq))
      }
      .minBy(_._2)
    bus * time
  }

  case class Bus(freq: Int, offset: Int)

  object Bus {
    def parse(input: String): List[Bus] =
      input
        .split(",")
        .zipWithIndex
        .collect {
          case (id, offset) if id.toIntOption.nonEmpty => Bus(id.toInt, offset)
        }
        .toList
  }

  def part2(buses: List[Bus]): BigInt =
    chineseRemainder(buses.map { bus =>
      BigInt(-bus.offset) -> BigInt(bus.freq.toLong)
    })._1

  def main(args: Array[String]): Unit = {
    val lines      = readInputLines(day = 13).toList
    val start      = lines.head.toInt
    val ids        = lines(1).split(",").toList
    val numericIds = ids.flatMap(_.toIntOption)
    println(part1(start, numericIds))
    println(part2(Bus.parse(lines(1))))
  }

}
