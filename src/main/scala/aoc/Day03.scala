package aoc

object Day03 {

  case class Area(trees: List[List[Boolean]]) {
    def lines: Int = trees.size

    def treeAt(pos: Pos): Boolean = {
      val row = trees(pos.r)
      row(pos.c % row.length)
    }

    override def toString: String =
      trees
        .map(_.map {
          case true  => '#'
          case false => '.'
        }.mkString)
        .mkString("\n")
  }

  object Area {
    def parse(lines: Iterator[String]): Area = Area(lines.map(parseLine).toList)

    private def parseLine(line: String): List[Boolean] = line.map(_ == '#').toList
  }

  def countTrees(area: Area, step: Pos): BigDecimal = {
    LazyList
      .iterate(Pos.Origin)(_ + step)
      .takeWhile(_.r < area.lines).count(pos => area.treeAt(pos))
  }

  def part1(area: Area): BigDecimal = countTrees(area, step = Pos(r = 1, c = 3))

  def part2(area: Area): BigDecimal =
    List(Pos(1, 1), Pos(1, 3), Pos(1, 5), Pos(1, 7), Pos(2, 1))
      .map(slope => countTrees(area, slope))
      .product

  def main(args: Array[String]): Unit = {
    val input = Area.parse(readInputLines(day = 3))
    println(part1(input))
    println(part2(input))
  }
}
