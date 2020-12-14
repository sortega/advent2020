package aoc

import aoc.Day14._
import org.junit.Assert._
import org.junit.Test

class Day14Test {

  @Test def part1Test(): Unit = {
    val commands = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                     |mem[8] = 11
                     |mem[7] = 101
                     |mem[8] = 0
                     |""".stripMargin.linesIterator.map(Command.parse).toList
    assertEquals(165, part1(commands))
  }
  
  @Test def part2Test(): Unit = {
    val commands = """mask = 000000000000000000000000000000X1001X
                     |mem[42] = 100
                     |mask = 00000000000000000000000000000000X0XX
                     |mem[26] = 1
                     |""".stripMargin.linesIterator.map(Command.parse).toList
    assertEquals(208, part2(commands))
  }

}
