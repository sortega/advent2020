package aoc

enum HexDir(val vector: Pos) {
  case e extends HexDir(Pos(0, 1))
  case se extends HexDir(Pos(1, 1))
  case sw extends HexDir(Pos(1, 0))
  case w extends HexDir(Pos(0, -1))
  case nw extends HexDir(Pos(-1, -1))
  case ne extends HexDir(Pos(-1, 0))
}
