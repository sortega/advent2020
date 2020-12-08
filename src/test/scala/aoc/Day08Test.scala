package aoc

import aoc.Day08._
import aoc.Day08.Instruction._
import org.junit.Assert._
import org.junit.Test

class Day08Test {

  private val listing = Listing.parse("""nop +0
                                        |acc +1
                                        |jmp +4
                                        |acc +3
                                        |jmp -3
                                        |acc -99
                                        |acc +1
                                        |jmp -4
                                        |acc +6
                                        |""".stripMargin.linesIterator)

  @Test def parsingTest(): Unit = {
    assertEquals(
      Listing(
        nop(0),
        acc(1),
        jmp(4),
        acc(3),
        jmp(-3),
        acc(-99),
        acc(1),
        jmp(-4),
        acc(6)
      ),
      listing
    )
  }

  @Test def machineTest(): Unit = {
    assertEquals(Machine(listing, pc = 1, accum = 5), Machine(listing).steps(7))
  }

  @Test def part1Test(): Unit = {
    assertEquals(5, part1(listing))
  }

  @Test def part2Test(): Unit = {
    assertEquals(8, part2(listing))
  }
}
