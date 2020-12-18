package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining._

object Day18 {
  enum Op {
    case Add
    case Mul
    
    def apply(l: Long, r: Long): Long = this match {
      case Add => l + r
      case Mul => l * r
    }
  }
  
  enum Expression {
    case Lit(value: Long)
    case Combine(op: Op, left: Expression, right: Expression)
    
    def eval: Long = this match {
      case Lit(value) => value
      case Combine(op, left, right) => op(left.eval, right.eval)
    }
  }
  
  enum Token {
    case Expr(value: Expression)
    case Symbol(op: Op)
    case LParen
    case RParen
  }
  
  import Token._
  
  object Expression {
    private val TokenPattern = """\d+|\(|\)|\+|\*""".r

    private def tokenize(input: String): List[Token] =
      TokenPattern.findAllIn(input).map {
        case "+" => Symbol(Op.Add)
        case "*" => Symbol(Op.Mul)
        case "(" => LParen
        case ")" => RParen
        case num => Expr(Lit(num.toLong))
      }.toList
      
    private def takeParenthesized(input: List[Token]): (List[Token], List[Token]) = {
      val size = input.map {
        case LParen => -1
        case RParen => 1
        case _ => 0
      }.scanLeft(-1)(_ + _).tail.segmentLength(_ < 0)
      val (parenthesized, RParen :: rest) = input.splitAt(size)
      (parenthesized, rest)
    }
    
    def parse(input: String): Expression = {
      def parseTokens(input: List[Token]): Expression = input match {
        case Nil => throw new IllegalArgumentException("empty expression")
          
        case Expr(single) :: Nil => single
          
        case Expr(left) :: Symbol(op) :: Expr(right) :: rest =>
          parseTokens(Expr(Combine(op, left, right)) :: rest)

        case Expr(left) :: Symbol(op) :: LParen :: rest =>
          val (parenthesized, afterParen) = takeParenthesized(rest)
          parseTokens(Expr(Combine(op, left, parseTokens(parenthesized))) :: afterParen)
          
        case LParen :: rest =>
          val (parenthesized, afterParen) = takeParenthesized(rest)
          parseTokens(Expr(parseTokens(parenthesized)) :: afterParen)
      }
      
      parseTokens(tokenize(input))
    }

    def parse2(input: String): Expression = {
      def parseTokens(input: List[Token]): Expression = { 
        if (input.contains(LParen)) {
          val (prefix, LParen :: afterLParen) = input.span(_ != LParen)
          val (parenthesized, suffix) = takeParenthesized(afterLParen)
          parseTokens(prefix ::: Expr(parseTokens(parenthesized)) :: suffix)
        } else if (input.contains(Symbol(Op.Add))) {
          val index = input.indexOf(Symbol(Op.Add))
          val Expr(left) = input(index - 1)
          val Expr(right) = input(index + 1)
          val prefix = input.slice(0, index - 1)
          val suffix = input.slice(index + 2, input.size)
          parseTokens(prefix ::: Expr(Combine(Op.Add, left, right)) :: suffix)
        } else input match {
          case Nil => throw new IllegalArgumentException("empty expression")
          case Expr(single) :: Nil => single
          case Expr(left) :: Symbol(op) :: Expr(right) :: rest =>
            parseTokens(Expr(Combine(op, left, right)) :: rest)
        }
      }

      parseTokens(tokenize(input))
    }
  }
  
  import Expression._

  def part1(input: List[Expression]): Long = input.map(_.eval).sum
  def part2(input: List[Expression]): Long = input.map(_.eval).sum

  def main(args: Array[String]): Unit = {
    println(part1(parseInputLines(day = 18)(Expression.parse)))
    println(part2(parseInputLines(day = 18)(Expression.parse2)))
  }
}
