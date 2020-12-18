package aoc

import aoc.Day18._
import org.junit.Assert._
import org.junit.Test

class Day18Test {

  @Test def part1Test(): Unit = {
    assertEquals(71, Expression.parse("1 + 2 * 3 + 4 * 5 + 6").eval)
    assertEquals(26, Expression.parse("2 * 3 + (4 * 5)").eval)
    assertEquals(437, Expression.parse("5 + (8 * 3 + 9 + 3 * 4 * 3)").eval)
    assertEquals(12240, Expression.parse("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))").eval)
    assertEquals(13632, Expression.parse("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").eval)
  }

  @Test def part2Test(): Unit = {
    assertEquals(231, Expression.parse2("1 + 2 * 3 + 4 * 5 + 6").eval)
    assertEquals(51, Expression.parse2("1 + (2 * 3) + (4 * (5 + 6))").eval)
    assertEquals(46, Expression.parse2("2 * 3 + (4 * 5)").eval)
    assertEquals(1445, Expression.parse2("5 + (8 * 3 + 9 + 3 * 4 * 3)").eval)
    assertEquals(669060, Expression.parse2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))").eval)
    assertEquals(23340, Expression.parse2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").eval)
  }
}
