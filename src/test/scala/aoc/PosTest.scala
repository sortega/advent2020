package aoc

import org.junit.Assert._
import org.junit.Test

class PosTest {

  @Test def multiplicationTest(): Unit = {
    assertEquals(Pos.Origin, Pos.Origin * Pos(1, 1))
    assertEquals(Pos(0, 1), Pos(0, 1) * Pos(0, 1))
    assertEquals(Pos(-1, 0), Pos(0, 1) * Pos(-1, 0))
    assertEquals(Pos(0, -1), Pos(-1, 0) * Pos(-1, 0))
    assertEquals(Pos(1, 0), Pos(-1, 0) * Pos(-1, 0) * Pos(-1, 0))
  }
  
}
