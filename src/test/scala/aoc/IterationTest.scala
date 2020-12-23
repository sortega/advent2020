package aoc

import aoc.Iteration._
import org.junit.Assert._
import org.junit.Test

class IterationTest {

  @Test def repeatedlyTest(): Unit = {
    assertEquals(1, repeatedly(1, -1)(_ + 1))
    assertEquals(1, repeatedly(1, 0)(_ + 1))
    assertEquals(2, repeatedly(1, 1)(_ + 1))
    assertEquals(3, repeatedly(1, 2)(_ + 1))
  }
  
}
