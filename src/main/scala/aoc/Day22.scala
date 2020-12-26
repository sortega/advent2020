package aoc

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.chaining._
import scala.util.matching.Regex

import aoc.CardinalPoint._
import aoc.Iteration.iterateUntil
import aoc.Pos.Syntax._
import com.google.ortools.Loader
import com.google.ortools.constraintsolver._

object Day22 {
  val verbose = false
  type Card = Int

  case class Player(deck: Queue[Card]) extends AnyVal {
    def lost: Boolean = deck.isEmpty
    def alive: Boolean = !lost

    def draw: (Card, Player) =
      deck.dequeueOption
        .map { (card, deck) =>
          (card, Player(deck))
        }
        .getOrElse(throw new IllegalArgumentException("Player already lost"))

    def keep(card: Card): Player = Player(deck.enqueue(card))

    def score: Int = deck.reverse
      .zip(LazyList.from(1))
      .map { case (card, factor) => card * factor }
      .sum

    override def toString: String = deck.mkString(", ")
  }
  object Player {
    def parse(lines: Iterator[String]): Player = Player(Queue.from(lines.map(_.toInt)))
  }

  case class Game(player1: Player, player2: Player) {
    def finished: Boolean = player1.lost || player2.lost
  }

  object Game {
    def parse(lines: Iterator[String]): Game = {
      val (beforeBreak, afterBreak) = lines.span(_.trim.nonEmpty)
      Game(Player.parse(beforeBreak.drop(1)), Player.parse(afterBreak.drop(2)))
    }
  }

  def log(message: => String, nesting: Int = 0): Unit = if (verbose) {
    println("  " * nesting + message)
  }

  def playGame(initial: Game): Player = {
    var game = initial
    var round = 1

    while(!game.finished) {
      val (card1, nextPlayer1) = game.player1.draw
      val (card2, nextPlayer2) = game.player2.draw
      log(s"-- Round $round --")
      log(s"Player 1's deck: ${game.player1}")
      log(s"Player 2's deck: ${game.player2}")
      log(s"Player 1 plays: $card1")
      log(s"Player 2 plays: $card2")

      game = if (card1 > card2) {
        log("Player 1 wins the round!\n")
        Game(
          player1 = nextPlayer1.keep(card1).keep(card2),
          player2 = nextPlayer2
        )
      } else {
        log("Player 2 wins the round!\n")
        Game(
          player1 = nextPlayer1,
          player2 = nextPlayer2.keep(card2).keep(card1)
        )
      }
      round += 1
    }
    
    log(s"== Post-game results ==")
    log(s"Player 1's deck: ${game.player1}")
    log(s"Player 2's deck: ${game.player2}")
    if (game.player1.alive) game.player1 else game.player2
  }

  def part1(input: Game): Int = playGame(input).score

  enum Outcome {
    case DejaVu
    case Completed(winner: Int, player: Player)
  }

  def playRecursiveGame(initial: Game, nesting: Int = 0): Outcome = {
    var game = initial
    val seenGames = mutable.HashSet.empty[Game]
    var round = 1

    while(!game.finished) {
      log(s"-- Round $round --", nesting)
      log(s"Player 1's deck: ${game.player1}", nesting)
      log(s"Player 2's deck: ${game.player2}", nesting)

      if (seenGames.contains(game)) {
        log("Deja vu!")
        return Outcome.DejaVu
      }
      seenGames += game

      val (card1, nextPlayer1) = game.player1.draw
      val (card2, nextPlayer2) = game.player2.draw
      val roundWinner: Int = if (card1 <= nextPlayer1.deck.size && card2 <= nextPlayer2.deck.size) {
        log("Playing a sub-game to determine the winner...\n", nesting)
        val recursiveGame = Game(
          Player(nextPlayer1.deck.take(card1)),
          Player(nextPlayer2.deck.take(card2))
        )
        playRecursiveGame(recursiveGame, nesting + 1) match {
          case Outcome.DejaVu => 1
          case Outcome.Completed(winner, _) => winner
        }
      } else {
        log(s"Player 1 plays: $card1", nesting)
        log(s"Player 2 plays: $card2", nesting)
        if (card1 > card2) 1 else 2
      }
      
      log(s"Player $roundWinner wins the round!\n", nesting)
      game = roundWinner match {
        case 1 => Game(
          player1 = nextPlayer1.keep(card1).keep(card2),
          player2 = nextPlayer2
        )
        case 2 => Game(
          player1 = nextPlayer1,
          player2 = nextPlayer2.keep(card2).keep(card1)
        )
      } 
      
      round += 1
    }

    log(s"== Post-game results ==", nesting)
    log(s"Player 1's deck: ${game.player1}", nesting)
    log(s"Player 2's deck: ${game.player2}", nesting)
    if (game.player1.alive) Outcome.Completed(winner = 1, game.player1)
    else Outcome.Completed(winner = 2, game.player2)
  }

  def part2(input: Game): Int = {
    val Outcome.Completed(_, player) = playRecursiveGame(input)
    player.score
  }

  def main(args: Array[String]): Unit = {
    val input = Game.parse(readInputLines(day = 22))
    println(part1(input))
    println(part2(input))
  }
}
