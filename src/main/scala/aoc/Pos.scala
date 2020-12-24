package aoc

case class Pos(r: Int, c: Int) {
  import Pos._
  
  def +(other: Pos)  = Pos(r + other.r, c + other.c)
  def *(scalar: Int) = Pos(r * scalar, c * scalar)

  /** Complex number multiplication assuming rows as the x axis
    *  and -cols as the y axis
    */
  def *(other: Pos) = Pos(
    r = c * other.r + r * other.c,
    c = c * other.c - r * other.r
  )

  def manhattanNorm: Int = r.abs + c.abs

  def adjacent: Set[Pos] = Adjacent.map(_ + this)
  
  def hexAdjacent: Set[Pos] = HexDir.values.map(_.vector + this).toSet

  def to(other: Pos): IndexedSeq[Pos] = for {
    row <- this.r to other.r
    col <- this.c to other.c
  } yield Pos(row, col)

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
