package aoc

import scala.util.chaining._

import aoc.Iteration.{iterateUntil, repeatedly}
import aoc.ModularArithmetic._

object Day23 {
  val verbose = false

  def rotate(values: Array[Int], pos: Int): Array[Int] = {
    val i               = pos mod values.length
    val (before, after) = values.splitAt(i)
    after ++ before
  }

  class Node(val value: Int, var next: Node) {
    def find(target: Int): Option[Node] = {
      var current = this
      while (current.value != target) {
        current = current.next
        if (current == null || current == this) {
          return None
        }
      }
      return Some(current)
    }

    def excise(size: Int): Node = {
      val excisionStart = next
      val excisionEnd   = repeatedly(excisionStart, size - 1)(_.next)
      next = excisionEnd.next
      excisionEnd.next = null
      excisionStart
    }

    def insert(nodes: Node): Unit = {
      val after = next
      next = nodes
      var current = nodes
      while(current.next != null) {
        current = current.next
      }
      current.next = after
    }

    def toLazyList: LazyList[Int] = value #:: Option(next).fold(LazyList.empty)(_.toLazyList)
  }

  case class Cups(head: Node, index: Map[Int, Node]) {

    def move: Cups = {
      if (verbose) { println(s"cups: $this") }
      val picked       = head.excise(3)
      val pickedValues = picked.toLazyList
      if (verbose) { println(s"picked up: ${pickedValues.mkString(", ")}") }
      val destinationValue = iterateUntil(head.value)(i => if (i == 1) index.size else i - 1) { i =>
        i != head.value && !pickedValues.contains(i)
      }
      if (verbose) { println(s"destination: $destinationValue\n") }
      val destination = index(destinationValue)
      destination.insert(picked)
      copy(head = head.next)
    }

    def move(times: Int): Cups = {
      val start = System.nanoTime()
      (1 to times).foldLeft(this) { (cups, i) =>
        if (i % 1000000 == 0) {
          val iterNanos = if (i > 0) (System.nanoTime() - start).toDouble / i else 0d
          val remaining      = ((times - i) * iterNanos / 1000000 / 1000).toInt
          println(s"-- move $i -- (${iterNanos.round} ns/iter, $remaining secs remaining)")
        }
        cups.move
      }
    }

    def numbersAfter1: LazyList[Int] = head.find(1).get.next.toLazyList

    def twoNumbersAfter1Multiplied: Long =
      numbersAfter1.take(2).map(_.toLong).product

    override def toString: String = {
      val buffer  = new StringBuilder(s"(${head.value})")
      var pointer = head.next
      while (pointer != head) {
        buffer.append(s" ${pointer.value}")
        pointer = pointer.next
      }
      buffer.toString
    }
  }

  object Cups {
    def parse(input: String): Cups = apply(input.map(_.toString.toInt))

    def apply(values: IndexedSeq[Int]): Cups = {
      val nodes = values.map(new Node(_, next = null))
      nodes.zip(nodes.tail :+ nodes.head).foreach { case (node, next) =>
        node.next = next
      }
      val index = nodes.map(n => n.value -> n).toMap
      Cups(nodes.head, index)
    }
  }

  def part1(input: String, moves: Int = 100): String =
    Cups.parse(input).move(moves).numbersAfter1.take(8).mkString

  def part2(input: String, moves: Int = 10000000): Long =
    Cups(input.map(_.toString.toInt) ++ (10 to 1000000)).move(moves).twoNumbersAfter1Multiplied

  def main(args: Array[String]): Unit = {
    val input = "158937462"
    println(part1(input))
    println(part2(input))
  }
}
