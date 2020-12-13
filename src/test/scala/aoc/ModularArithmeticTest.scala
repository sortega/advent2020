package aoc

import aoc.ModularArithmetic._
import org.junit.Assert._
import org.junit.Test

class ModularArithmeticTest {

  @Test def modTest(): Unit = {
    assertEquals(5, 15 mod 10)
    assertEquals(4, -6 mod 10)
  }

  @Test def gcdTest(): Unit = {
    assertEquals(3, gcd(15, 9))
    assertEquals(1, gcd(13, 21))
    assertEquals(30, gcd(120, 150))
  }

  @Test def extendedGcdTest(): Unit = {
    assertEquals((2, -9, 47), extendedGcd(240, 46))
    assertEquals((3, -1, 2), extendedGcd(15, 9))
    assertEquals((1, 5, -8), extendedGcd(21, 13))
    assertEquals((30, 1, -1), extendedGcd(150, 120))
  }

  @Test def chineseRemainderTest(): Unit = {
    assertEquals((39, 60), chineseRemainder(List(0 -> 3, 3 -> 4, 4 -> 5)))
    assertEquals((1275, 4199), chineseRemainder(List(0 -> 17, 1 -> 13, 2 -> 19)))
  }
}
