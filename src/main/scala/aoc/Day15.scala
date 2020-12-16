package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day15 {

  case class State(lastSeen: Map[Int, Int], last: Int, index: Int) {
    def step: (Int, State) = {
      val current: Int = lastSeen
        .get(last)
        .fold(0)(index - _)
      val nextState = State(
        lastSeen.updated(last, index),
        current,
        index + 1
      )
      (current, nextState)
    }
  }

  object State {
    def init(prefix: List[Int]): State = {
      val lastSeen = prefix.init.zipWithIndex.foldLeft(Map.empty[Int, Int]) {
        case (lastSeen, (number, index)) =>
          lastSeen.updated(number, index + 1)
      }
      State(lastSeen, last = prefix.last, index = prefix.size)
    }
  }

  def memoryGameSequence(prefix: List[Int]): LazyList[Int] =
    prefix.to(LazyList) #::: LazyList.unfold(State.init(prefix))(s => Some(s.step))

  def part1(input: List[Int]): Int = memoryGameSequence(input).drop(2019).head
  
  def part2(input: List[Int]): Int = memoryGameSequence(input).drop(30000000 - 1).head

  def main(args: Array[String]): Unit = {
    val input = List(0, 14, 1, 3, 7, 9)
    println(part1(input))
    timed(println(part2(input)))
  }
}
