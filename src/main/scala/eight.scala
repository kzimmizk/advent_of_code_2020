import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by kzimmerman on 12/8/20.
  */
object eight {

  case class State(counter: Int, accumulator: Int)

  trait Instruction {
    def run(state: State): State
    def flip(): Instruction
  }

  class NOP(val amount: Int) extends Instruction {
    def run(state: State): State = State(state.counter + 1, state.accumulator)
    def flip(): Instruction = new JMP(amount)
  }

  class ACC(val amount: Int) extends Instruction {
    def run(state: State): State = State(state.counter + 1, state.accumulator + amount)
    def flip(): Instruction = new ACC(amount)
  }

  class JMP(val amount: Int) extends Instruction {
    def run(state: State): State = State(state.counter + amount, state.accumulator)
    def flip(): Instruction = new NOP(amount)
  }

  object Reader {
    val rule: Regex = raw"^([a-zA-Z]{3}) (.*)$$".r
    def apply(in: String): Instruction = {
      in match {
        case rule(command, amount) =>
          if (command == "acc") {
            new ACC(Integer.valueOf(amount))
          } else if (command == "jmp") {
            new JMP(Integer.valueOf(amount))
          } else {
            new NOP(Integer.valueOf(amount))
          }
      }
    }
  }

  class Runner {
    private var seen: Set[Int] = Set[Int]()
    private var state: State = State(0, 0)
    def run(program: Array[Instruction], changed: Int): Int = {
      var found: Boolean = false
      while (!seen.contains(state.counter) && state.counter != program.length) {
        seen = seen + state.counter
        state = program(state.counter).run(state)
        if (state.counter == program.length) {
          found = true
          println(s"Second Answer: ${state.accumulator}")
        }
      }
      state.accumulator
    }
  }
  val program: Array[Instruction] = Source.fromFile("./src/main/data/8").getLines().map(l => Reader(l)).toArray

  def main(args: Array[String]): Unit = {
    val firstAnswer: Int = new Runner().run(program, 0)
    println(s"First Answer: $firstAnswer")
    for (i <- program.indices) {
      val testProgram = program.clone()
      program.update(i, program(i).flip())
      new Runner().run(program, i)
      program.update(i, program(i).flip())
    }
  }
}
