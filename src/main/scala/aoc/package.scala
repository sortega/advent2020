package aoc

import scala.io.{BufferedSource, Source}

def readInputLines(day: Int): Iterator[String] = 
  Source.fromInputStream(getClass.getResourceAsStream(f"/day$day%02d.txt")).getLines()

def parseInputLines[A](day: Int)(parse: String => A): List[A] = readInputLines(day).map(parse).toList