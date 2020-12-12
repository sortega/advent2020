package aoc

enum Side {
  case Left
  case Right

  def sign: Int = this match {
    case Left => -1
    case Right => 1
  }
}