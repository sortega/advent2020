package aoc

import scala.annotation.tailrec

object Iteration {
  def repeatedly[A](a: A, times: Int)(f: A => A): A = {
    @tailrec
    def go(a: A, times: Int): A =
      if (times > 0) go(f(a), times - 1) else a
    go(a, times)
  }
  
  def iterateUntil[A](a: A)(f: A => A)(pred: A => Boolean): A = {
    @tailrec def go(a: A): A = if (pred(a)) a else go(f(a))
    go(a)
  }
}
