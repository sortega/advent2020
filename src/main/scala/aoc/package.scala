package aoc

import scala.io.{BufferedSource, Source}

def readInputLines(day: Int, test: Boolean = false): Iterator[String] = {
  val name = "/day%02d%s.txt".format(day, if (test) "_test" else "")
  Source.fromInputStream(getClass.getResourceAsStream(name)).getLines()
}

def parseInputLines[A](day: Int, test: Boolean = false)(parse: String => A): List[A] =
  readInputLines(day, test).map(parse).toList

def parseInputGroupedLines[A](day: Int, test: Boolean = false)(parse: List[String] => A): List[A] =
  groupByEmptyLines(readInputLines(day, test).to(LazyList)).map(parse).toList

def groupByEmptyLines(iterator: LazyList[String]): LazyList[List[String]] =
  if (iterator.isEmpty) LazyList.empty
  else {
    val (firstParagraph, rest) = iterator.span(_.strip().nonEmpty)
    firstParagraph.toList #:: groupByEmptyLines(rest.drop(1))
  }
