package aoc

enum CardinalPoint(val vector: Pos) {
  case North extends CardinalPoint(Pos(-1, 0))
  case East extends CardinalPoint(Pos(0, 1))
  case South extends CardinalPoint(Pos(1, 0))
  case West extends CardinalPoint(Pos(0, -1))

  def turnRight: CardinalPoint = this match {
    case North => East
    case East => South
    case South => West
    case West => North
  }

  /** Counterclockwise addition */
  def +(other: CardinalPoint): CardinalPoint = {
    val rotatedVector = vector * other.vector
    CardinalPoint.values.find(_.vector == rotatedVector).get
  }
}

object CardinalPoint {
  def fromDegrees(degrees: Int): CardinalPoint = {
    require(degrees % 90 == 0)
    mod(degrees / 90, 4) match {
      case 0 => East
      case 1 => North
      case 2 => West
      case 3 => South
    }
  }

  private def mod(a: Int, b: Int): Int = {
    val r = a % b
    if (r >= 0) r else r + b
  }
}