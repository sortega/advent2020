package aoc

object Day05 {
  case class BoardingPass(row: Int, col: Int) {
    def seatId: Int = row * 8 + col
  }

  object BoardingPass {
    private val Mapping = Map(
      'L' -> '0',
      'R' -> '1',
      'F' -> '0',
      'B' -> '1'
    )

    def parse(input: String): BoardingPass = {
      val (row, col) = input.map(Mapping).splitAt(7)
      BoardingPass(Integer.parseInt(row, 2), Integer.parseInt(col, 2))
    }
  }
  
  implicit class BoardingPassInterpolator(val sc: StringContext) extends AnyVal {
    def pass(args: Any*): BoardingPass = BoardingPass.parse(sc.s(args: _*))
  }

  def part1(passes: List[BoardingPass]): Int = passes.map(_.seatId).max

  def findHoles(numbers: List[Int]): Iterator[Int] =
    numbers.sorted.iterator
      .sliding(size = 2, step = 1)
      .collect {
        case Seq(prev, next) if prev + 2 == next => prev + 1
      }

  def part2(passports: List[BoardingPass]): Int =
    findHoles(passports.map(_.seatId)).next()

  def main(args: Array[String]): Unit = {
    val input = parseInputLines(day = 5)(BoardingPass.parse)
    println(part1(input))
    println(part2(input))
  }
}
