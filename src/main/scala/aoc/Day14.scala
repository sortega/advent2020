package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 {
  enum Command {
    case SetMask(mask: String)
    case Assign(address: Long, value: Long)
  }

  object Command {
    def parse(input: String): Command = input match {
      case s"mask = $mask" => SetMask(mask)
      case s"mem[$addess] = $value" => Assign(addess.toLong, value.toLong)
    }
  }

  case class OverwriteMask(packed: String, mask: Long, value: Long) {
    def maskWord(word: Long): Long = (mask & value) | ((~mask) & word)

    def maskAddress(address: Long): List[Long] = {
      @tailrec
      def go(mask: List[Char], address: List[Char], decoded: List[String] = List("")): List[Long] =
        (mask, address) match {
          case ('0' :: maskRest, bit :: addressRest) =>
            go(maskRest, addressRest, decoded.map(value => s"$bit$value"))
          case ('1' :: maskRest, _ :: addressRest) =>
            go(maskRest, addressRest, decoded.map(value => s"1$value"))
          case ('X' :: maskRest, _ :: addressRest) =>
            go(maskRest, addressRest, decoded.flatMap(value =>  List(s"1$value", s"0$value")))
          case (Nil, Nil) => decoded.map(parseBinary)
          case _ => throw new IllegalStateException()
        }

      go(packed.reverse.toList, address.toBinaryString.reverse.padTo(36, '0').toList)
    }
  }

  case object OverwriteMask {
    def apply(packed: String): OverwriteMask = {
      val mask = parseBinary(packed.map {
        case 'X' => '0'
        case _ => '1'
      })
      val value = parseBinary(packed.replaceAll("X", "0").nn)
      OverwriteMask(packed, mask, value)
    }
  }

  private def parseBinary(string: String): Long = BigInt(string, 2).toLong

  case class State(mask: OverwriteMask, memory: Map[Long, Long]) {
    def run(command: Command): State = command match {
      case Command.SetMask(stringMask)    => copy(mask = OverwriteMask(stringMask))
      case Command.Assign(address, value) => copy(memory = memory.updated(address, mask.maskWord(value)))
    }

    def runV2(command: Command): State = command match {
      case Command.SetMask(stringMask)    => copy(mask = OverwriteMask(stringMask))
      case Command.Assign(address, value) =>
        copy(memory = mask.maskAddress(address).foldLeft(memory) { (memory, address) =>
          memory.updated(address, value)
        })
    }
  }

  object State {
    val Initial = State(OverwriteMask("X" * 36, 0, 0), Map.empty)
  }

  def part(commands: List[Command])(run: (State, Command) => State): Long = {
    val state = commands.foldLeft(State.Initial)(run)
    state.memory.values.sum
  }

  def part1(commands: List[Command]): Long = part(commands)(_.run(_))
  def part2(commands: List[Command]): Long = part(commands)(_.runV2(_))

  def main(args: Array[String]): Unit = {
    val input = parseInputLines(day = 14)(Command.parse)
    println(part1(input))
    println(part2(input))
  }
}
