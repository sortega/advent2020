package aoc

import aoc.Day21._
import org.junit.Assert._
import org.junit.Test

class Day21Test {
  val input = parseInputLines(day = 21, test = true)(Recipe.parse)
  val alergens = Map(
    "mxmxvkd" -> Some("dairy"),
    "sqjhc"   -> Some("fish"),
    "fvjkl"   -> Some("soy"),
    "trh"     -> None,
    "nhms"    -> None,
    "kfcds"   -> None,
    "sbzzf"   -> None
  )

  @Test def findAlergensTest(): Unit = {
    assertEquals(List(alergens), findAlergens(input))
  }

  @Test def part1Test(): Unit = {
    assertEquals(5, part1(input, alergens))
  }

  @Test def part2Test(): Unit = {
    assertEquals("mxmxvkd,sqjhc,fvjkl", part2(alergens))
  }
}
