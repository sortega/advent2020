package aoc

import scala.collection.mutable

object Memo {
  // Arity 1

  def apply[A, B](f: A => B): A => B = {
    val cache = mutable.HashMap[A, B]()
    a => cache.getOrElseUpdate(a, f(a))
  }

  def recursive[A, B](f: (A, A => B) => B): A => B = {
    lazy val memoized: A => B = Memo(a => f(a, memoized))
    memoized
  }

  // Arity 2

  def apply[A, B, C](f: (A, B) => C): (A, B) => C = {
    val cache = mutable.HashMap[(A, B), C]()
    (a, b) => cache.getOrElseUpdate((a, b), f(a, b))
  }

  def recursive[A, B, C](f: (A, B, (A, B) => C) => C): (A, B) => C = {
    lazy val memoized: (A, B) => C = Memo((a, b) => f(a, b, memoized))
    memoized
  }

  // Arity 3

  def apply[A, B, C, D](f: (A, B, C) => D): (A, B, C) => D = {
    val cache = mutable.HashMap[(A, B, C), D]()
    (a, b, c) => cache.getOrElseUpdate((a, b, c), f(a, b, c))
  }

  def recursive[A, B, C, D](f: (A, B, C, (A, B, C) => D) => D): (A, B, C) => D = {
    lazy val memoized: (A, B, C) => D = Memo((a, b, c) => f(a, b, c, memoized))
    memoized
  }
}
