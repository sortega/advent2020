package aoc

import scala.annotation.tailrec
import scala.math.Integral.Implicits._
import scala.math.Ordering.Implicits._

object ModularArithmetic {
  @tailrec
  def gcd[@specialized(Int, Long) N: Integral](a: N, b: N): N = {
    val zero = Integral[N].zero
    if (a < zero || b < zero) gcd(a.abs, b.abs)
    else if (a < b) gcd(b, a)
    else if (b == zero) a
    else gcd(b, a mod b)
  }

  def extendedGcd[@specialized(Int, Long) N: Integral](a: N, b: N): (N, N, N) = {
    val zero = Integral[N].zero
    val one  = Integral[N].one
    require(a > b && a > zero && b > zero)
    @tailrec def go(
        a: N,
        b: N,
        sa: N,
        sb: N,
        ta: N,
        tb: N
      ): (N, N, N) =
      if (b == zero) (a, sa, ta)
      else {
        val q = a / b
        go(
          a = b,
          b = a - q * b,
          sa = sb,
          sb = sa - q * sb,
          ta = tb,
          tb = ta - q * tb
        )
      }

    go(a, b, one, zero, zero, one)
  }

  @tailrec
  def chineseRemainder[@specialized(Int, Long) N: Integral](constraints: List[(N, N)]): (N, N) =
    constraints match {
      case Nil          => throw new IllegalArgumentException("at least one constraint is needed")
      case List((a, n)) => (a mod n, n)
      case (a1, n1) :: (a2, n2) :: rest if n1 < n2 =>
        chineseRemainder((a2, n2) :: (a1, n1) :: rest)
      case (a1, n1) :: (a2, n2) :: rest =>
        val (1, m1, m2) = extendedGcd(n1, n2)
        val x           = a1 * m2 * n2 + a2 * m1 * n1
        chineseRemainder((x mod (n1 * n2), n1 * n2) :: rest)
    }

  extension[N](n: N) {
    def mod(m: N)(implicit N: Integral[N]): N = {
      val r = n % m
      if (r >= N.zero) r else r + m
    }
  }
}
