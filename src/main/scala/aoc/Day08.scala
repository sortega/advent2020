package aoc

import scala.annotation.tailrec

object Day08 {
  enum Instruction(operand: Int) {
    case acc(addend: Int) extends Instruction(addend)
    case jmp(offset: Int) extends Instruction(offset)
    case nop(unused: Int) extends Instruction(unused)
  }

  import Instruction._

  type Listing = List[Instruction]
  object Listing {
    private val InstructionPattern = """(\w+) ([+-]\d+)""".r
    def apply(instructions: Instruction*): Listing = instructions.toList

    def parse(input: Iterator[String]): Listing = {
      input.map {
        case InstructionPattern("acc", operand) => acc(operand.toInt)
        case InstructionPattern("jmp", operand) => jmp(operand.toInt)
        case InstructionPattern("nop", operand) => nop(operand.toInt)
      }.toList
    }
  }

  enum Outcome {
    case Looped(firstRepeatingState: Machine)
    case Completed(message: String, lastState: Machine)
  }

  case class Machine(listing: Listing, pc: Int = 0, accum: Int = 0) {
    def run: Outcome = {
      @tailrec def go(machine: Machine, seenPCs: Set[Int] = Set.empty): Outcome = {
        if (seenPCs.contains(machine.pc)) Outcome.Looped(machine)
        else machine.step match {
          case Left(message) => Outcome.Completed(message, machine)
          case Right(nextMachine) => go(nextMachine, seenPCs = seenPCs + machine.pc)
        }
      }

      go(this)
    }

    def step: Either[String, Machine] = for {
      instruction <- fetch
    } yield instruction match {
      case acc(addend) => copy(pc = pc + 1, accum = accum + addend)
      case jmp(offset) => copy(pc = pc + offset)
      case nop(offset) => copy(pc = pc + 1)
    }

    def fetch: Either[String, Instruction] =
      if (listing.indices.contains(pc)) Right(listing(pc))
      else Left(s"Invalid memory access: $pc")

    def steps: LazyList[Machine] =
      this #:: LazyList.unfold(this)(_.step.toOption.map(m => (m, m)))
  }

  def part1(input: List[Instruction]): Int = {
    val Outcome.Looped(machine) = Machine(input).run
    machine.accum
  }

  def patchedListings(listing: Listing): Set[Listing] = listing.zipWithIndex
    .collect {
      case (jmp(_), offset) => listing.updated(offset, nop(0))
    }
    .toSet

  def part2(input: List[Instruction]): Int = {
    patchedListings(input).map(listing => Machine(listing).run).collectFirst {
      case Outcome.Completed(_, machine) => machine.accum
    }.get
  }

  def main(args: Array[String]): Unit = {
    val input = Listing.parse(readInputLines(day = 8))
    println(part1(input))
    println(part2(input))
  }
}
