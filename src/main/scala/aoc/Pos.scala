package aoc

import aoc.Pos.Adjacent

case class Pos(r: Int, c: Int) {
  def +(other: Pos)             = Pos(r + other.r, c + other.c)

  def adjacent: Set[Pos] = Adjacent.map(_ + this)

  override def toString: String = s"($r,$c)"
}

object Pos {
  val Origin = Pos(0, 0)

  // format: off
  val Adjacent = Set(
    Pos(-1, -1), Pos(-1, 0), Pos(-1, 1),
    Pos( 0, -1),             Pos( 0, 1),
    Pos( 1, -1), Pos( 1, 0), Pos( 1, 1)
  )
  // format: on
}
