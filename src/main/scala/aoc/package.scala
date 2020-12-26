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
    val (firstParagraph, rest) = iterator.span(_.trim.nonEmpty)
    firstParagraph.toList #:: groupByEmptyLines(rest.drop(1))
  }

def timed[A](block: => A): A = {
  val start = System.nanoTime()
  try block
  finally {
    println("(in %f ms)".format((System.nanoTime() - start) / 1000000f))
  }
}

extension[A, B] (elems: IterableOnce[A]) {
  def keyBy(f: A => B): Map[B, A] = elems.iterator.map { a =>
    f(a) -> a
  }.toMap
  
  def withValues(f: A => B): Map[A, B] = elems.iterator.map { a =>
    a -> f(a)
  }.toMap
}