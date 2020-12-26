package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._

import aoc.CardinalPoint._
import aoc.Iteration.repeatedly
import aoc.Pos.Syntax._

object Day20 {
  type Matrix[A] = List[List[A]]

  object Matrix {
    def tabulate[A](rows: Int, cols: Int)(f: (Int, Int) => A): Matrix[A] =
      (0 until rows).map { row =>
        (0 until cols).map { col =>
          f(row, col)
        }.toList
      }.toList
  }

  object Orientation {
    def apply(rotations: Int, horizFlip: Boolean): Int =
      rotations + (if (horizFlip) 4 else 0)

    def unapply(orientation: Int): Option[(Int, Boolean)] =
      if (orientation >= 0 && orientation < 16) Some((orientation % 4, (orientation & 4) != 0))
      else None

    val Range = 0 until 8
  }

  case class Tile(
      id: Int,
      lines: List[String],
      rotated: Int = 0,
      horizFlip: Boolean = false) {
    lazy val borders: Map[CardinalPoint, Int] = {
      val (west, east) = lines.map(l => (l.head, l.last)).unzip
      Map(
        North -> lines.head,
        South -> lines.last.reverse,
        East  -> east.mkString,
        West  -> west.mkString.reverse
      ).map { case (dir, chars) =>
        dir -> Integer.parseInt(chars.replaceAll("#", "1").replaceAll("\\.", "0"), 2)
      }
    }

    def horizontalFlip: Tile = copy(lines = lines.map(_.reverse), horizFlip = !horizFlip)

    def rotate: Tile = copy(
      lines = lines.transpose.map(_.mkString.reverse),
      rotated = (rotated + 1) % 4
    )

    def withOrientation(orientation: Int): Tile = {
      val Orientation(rotations, horizFlip) = orientation
      this
        .pipe(repeatedly(_, rotations)(_.rotate))
        .pipe(repeatedly(_, if (horizFlip) 1 else 0)(_.horizontalFlip))
    }

    def trimmed: List[String] = lines.slice(1, 9).map(_.slice(1, 9))

    override def toString: String =
      lines.mkString(
        "Tile %d (rot=%d, flip=%s):\nBorders: %s\n".format(id, rotated, horizFlip, borders),
        "\n",
        "\n"
      )
  }

  object Tile {
    def parseAll(input: Iterator[String]): List[Tile] = input
      .grouped(12)
      .map { tileInput =>
        val s"Tile $id:" :: rest = tileInput.toList
        val lines                = rest.filterNot(_.trim.isEmpty)
        Tile(id.toInt, lines)
      }
      .toList
  }

  case class Puzzle(tiles: Matrix[Tile]) extends AnyVal {
    def assembledWithBorders: String =
      tiles
        .flatMap { tileRow =>
          "" ::
            tileRow
              .map { tile =>
                tile.lines.map(_ + " ")
              }
              .transpose
              .map(_.mkString)
        }
        .mkString("\n")

    def assembled: String =
      tiles
        .flatMap { tileRow =>
          tileRow
            .map(_.trimmed)
            .transpose
            .map(_.mkString)
        }
        .mkString("\n")
  }

  val MatchingBorders: Map[Int, Int] = (0 to 1023).map { from =>
    from ->
      Integer.parseInt(from.toBinaryString.reverse.padTo(10, '0'), 2)
  }.toMap

  def findMatching(tiles: Set[Tile], border: Int): Set[Tile] =
    tiles.collect {
      case tile if tile.borders.valuesIterator.contains(border)                  => tile
      case tile if tile.borders.valuesIterator.contains(MatchingBorders(border)) => tile
    }

  private def solve(input: List[Tile]): Puzzle = {
    val n         = Math.sqrt(input.length).toInt
    val tiles     = input.toSet
    val tilesById = input.keyBy(_.id)

    val matchingTiles: Map[Int, Map[CardinalPoint, Int]] = (for {
      tile <- tiles
    } yield tile.id -> (for {
      (dir, border) <- tile.borders
      matching = findMatching(tiles - tile, border).map(_.id)
      if matching.nonEmpty
    } yield {
      require(matching.size == 1)
      (dir, matching.head)
    }).toMap).toMap

    def matchingId(dir: CardinalPoint, from: Tile): Option[Int] = matchingTiles(from.id).get(
      (dir match {
        case West if from.horizFlip => East
        case East if from.horizFlip => West
        case other                  => other
      }).pipe(repeatedly(_, (4 - from.rotated) % 4)(_.turnRight))
    )

    // Assemble puzzle
    val assembledTiles = mutable.Map.empty[Pos, Tile]

    // Rest of tiles
    for {
      row <- 0 until n
      col <- 0 until n
      pos = Pos(row, col)
    } {
      val northBorder =
        assembledTiles.get(Pos(row - 1, col)).map(_.borders(South).pipe(MatchingBorders))
      val westBorder =
        assembledTiles.get(Pos(row, col - 1)).map(_.borders(East).pipe(MatchingBorders))
      val matchingSides = List(row != 0, col != 0, row < n - 1, col < n - 1).count(identity)
      val candidates = (tilesById -- assembledTiles.valuesIterator.map(_.id)).values.toSet
        .pipe { (tiles: Set[Tile]) =>
          tiles.filter(tile => matchingTiles(tile.id).size == matchingSides)
        }
        .pipe { tiles =>
          northBorder.fold(tiles)(border => findMatching(tiles, border))
        }
        .pipe { tiles =>
          westBorder.fold(tiles)(border => findMatching(tiles, border))
        }

      require(candidates.size >= 1, s"${candidates.mkString("[", ", ", "]")} found")
      val candidate = candidates.head

      val temp1 = Orientation.Range.map(candidate.withOrientation)
      val temp2 = temp1
        .filter { tile =>
          northBorder match {
            case Some(border) => tile.borders(North) == border
            case None         => matchingId(North, tile).isEmpty
          }
        }
      val orientedTiles = temp2
        .filter { tile =>
          westBorder match {
            case Some(border) => tile.borders(West) == border
            case None         => matchingId(West, tile).isEmpty
          }
        }
      require(orientedTiles.size > 0, s"Oriented tiles: ${orientedTiles.mkString("[", ",", "]")}")
      val orientedTile = orientedTiles.head

      assembledTiles.put(pos, orientedTile)
    }

    Puzzle(
      Matrix.tabulate(n, n) { (row, col) =>
        assembledTiles(Pos(row, col))
      }
    )
  }

  def part1(input: List[Tile]): Long = {
    val puzzle = solve(input)
    println(puzzle.assembledWithBorders)
    List(
      puzzle.tiles.head.head,
      puzzle.tiles.head.last,
      puzzle.tiles.last.head,
      puzzle.tiles.last.last
    ).map(_.id.toLong).product
  }

  private def toPointSet(bitmap: String): Set[Pos] =
    (for {
      (line, row) <- bitmap.linesIterator.zipWithIndex
      (char, col) <- line.zipWithIndex
      if char == '#'
    } yield Pos(row, col)).toSet

  val MonsterPoints = toPointSet("""..................#.
                                   |#....##....##....###
                                   |.#..#..#..#..#..#...
                                   |""".stripMargin)

  def findPattern(bitmap: Set[Pos], pattern: Set[Pos]): Set[Pos] = {
    val numberedPoints = pattern.zipWithIndex.toMap
    val possiblePatterns: Map[Pos, Set[Int]] = (for {
      point           <- bitmap
      (delta, number) <- numberedPoints
    } yield (point - delta, number))
      .groupMapReduce(_._1)(pair => Set(pair._2))(_ union _)
    possiblePatterns.collect {
      case (pos, points) if points.size == pattern.size => pos
    }.toSet
  }

  def rotate(points: Set[Pos]): Set[Pos]        = points.map(_ * Pos(-1, 0))
  def flip(points: Set[Pos]): Set[Pos]          = points.map(p => p.copy(c = -p.c))
  def toString(points: Set[Pos]): String = {
    val minRow = points.map(_.r).min
    val maxRow = points.map(_.r).max
    val minCol = points.map(_.c).min
    val maxCol = points.map(_.c).max
    (minRow to maxRow)
      .map { row =>
        (minCol to maxCol).map { col =>
          if (points(Pos(row, col))) '#' else '.'
        }.mkString
      }
      .mkString("\n")
  }

  def part2(input: List[Tile]): Int = {
    val seaMap = solve(input).assembled
    println()
    println(seaMap)

    val bitmap = toPointSet(seaMap)
    val patterns = List(
      MonsterPoints,
      MonsterPoints.pipe(rotate),
      MonsterPoints.pipe(rotate).pipe(rotate),
      MonsterPoints.pipe(rotate).pipe(rotate).pipe(rotate)
    ).flatMap { pattern =>
      List(pattern, flip(pattern))
    }

    val numMonsters = (for {
      pattern <- patterns
      results = findPattern(bitmap, pattern)
      _       = println(results)
    } yield results.size).sum
    bitmap.size - numMonsters * MonsterPoints.size
  }

  def main(args: Array[String]): Unit = {
    val input = Tile.parseAll(readInputLines(day = 20))
    println(part1(input))
    println(part2(input))
  }
}
