
// Generator script for SNUSP
//
// Being used for Problem 179.
//
// Ended up being too slow for 179 (all the dynamic array manipulation
// would be far too much overhead), but this will be useful in the
// future.

import scala.collection.mutable.{Map, HashMap}

object Snusp {

  // Note: We're using Modular SNUSP, so we get ENTER and LEAVE but we
  // don't have UP, DOWN, SPLIT, or RAND.
  object Chars {
    val Left = '<'
    val Right = '>'
    val Incr = '+'
    val Decr = '-'
    val Read = ','
    val Write = '.'
    val Lurd = '\\'
    val Ruld = '/'
    val Skip = '!'
    val Skipz = '?'
    val Enter = '@'
    val Leave = '#'
  }

  case class Point(x: Int, y: Int) {
    def +(that: Point): Point = Point(this.x + that.x, this.y + that.y)
    def -(that: Point): Point = Point(this.x - that.x, this.y - that.y)
  }

  class OutputMap() extends Map[Point, Char] {
    private val impl = HashMap[Point, Char]()

    export impl.{iterator, get}

    override def addOne(elem: (Point, Char)): this.type = {
      impl += elem
      this
    }

    override def subtractOne(elem: Point): this.type = {
      impl -= elem
      this
    }

    def width: Int =
      impl.keys.map(_.x).max + 1

    def height: Int =
      impl.keys.map(_.y).max + 1

    def toCode(): String = {
      val builder = StringBuilder()
      val rowWidth = width
      val colHeight = height
      for (y <- 0 until colHeight) {
        for (x <- 0 until rowWidth) {
          builder.append(impl.getOrElse(Point(x, y), ' '))
        }
        builder.append('\n')
      }
      builder.toString
    }

  }

  class Compiler() {
    val outputMap = OutputMap()
    var sourcePosition = Point(0, 0)
    var memoryPosition = 0

    // How deeply nested we are in loops right now. The minimum depth
    // is 1, which means a new loop or conditional must go down one
    // row to safely branch. Each time we enter a loop, we increment
    // this value.
    var blockDepth = 1

    // Append one instruction and update sourcePosition.
    def append(char: Char): Unit = {
      outputMap(sourcePosition) = char
      sourcePosition += Point(1, 0)
    }

    // Seek to the given memory location. Updates sourcePosition and
    // memoryPosition.
    def seek(position: Int): Unit = {
      if (memoryPosition < position) {
        for (i <- 1 to position - memoryPosition) {
          append(Chars.Right)
        }
      } else if (memoryPosition > position) {
        for (i <- 1 to memoryPosition - position) {
          append(Chars.Left)
        }
      }
      memoryPosition = position
    }

    def inBlock[R](body: => R): R = {
      blockDepth += 1
      try {
        body
      } finally {
        blockDepth -= 1
      }
    }

  }

  trait Instruction {
    def compileTo(compiler: Compiler): Unit
  }

  class SimpleInstruction(char: Char) extends Instruction {
    def compileTo(compiler: Compiler): Unit = {
      compiler.append(char)
    }
  }

  val Incr = SimpleInstruction(Chars.Incr)
  val Decr = SimpleInstruction(Chars.Decr)
  val Read = SimpleInstruction(Chars.Read)
  val Write = SimpleInstruction(Chars.Write)

  class Seek(position: Int) extends Instruction {
    def compileTo(compiler: Compiler): Unit = {
      compiler.seek(position)
    }
  }

  class Progn(instructions: Instruction*) extends Instruction {
    def compileTo(compiler: Compiler): Unit = {
      for (i <- instructions) {
        i.compileTo(compiler)
      }
    }
  }

  // IfZero auto-adjusts the seek position at the end to match its
  // position at the beginning of the if statement.
  class IfZero(body: Instruction*) extends Instruction {
    def compileTo(compiler: Compiler): Unit = {
      val currentMemoryPos = compiler.memoryPosition
      val depth = compiler.blockDepth
      compiler.append(Chars.Skipz)
      compiler.outputMap(compiler.sourcePosition + Point(0, depth)) = Chars.Lurd
      compiler.append(Chars.Lurd)
      compiler.inBlock {
        for (i <- body) {
          i.compileTo(compiler)
        }
      }
      compiler.seek(currentMemoryPos)
      compiler.append(Chars.Skip)
      compiler.outputMap(compiler.sourcePosition + Point(0, depth)) = Chars.Ruld
      compiler.append(Chars.Ruld)
    }
  }

  // DoWhileNonzero throws if the seek position at the end doesn't match the
  // seek position at the beginning.
  class DoWhileNonzero(body: Instruction*) extends Instruction {
    def compileTo(compiler: Compiler): Unit = {
      val startSourcePos = compiler.sourcePosition
      val startMemoryPos = compiler.memoryPosition
      val depth = compiler.blockDepth
      compiler.append(Chars.Skip)
      compiler.outputMap(compiler.sourcePosition + Point(0, depth)) = Chars.Lurd
      compiler.append(Chars.Ruld)
      compiler.inBlock {
        for (i <- body) {
          i.compileTo(compiler)
        }
      }
      if (startMemoryPos != compiler.memoryPosition) {
        throw IllegalStateException(s"Seek position at end does not match start, start = ${startMemoryPos}, end = ${compiler.memoryPosition} for the loop from ${startSourcePos} to ${compiler.sourcePosition}.")
      }
      compiler.append(Chars.Skipz)
      compiler.outputMap(compiler.sourcePosition + Point(0, depth)) = Chars.Ruld
      compiler.append(Chars.Lurd)
    }
  }

  def repeat(instr: Instruction, count: Int) =
    Progn((1 to count).map(_ => instr): _*)

  val SetToZero = DoWhileNonzero(Decr)

  def setTo(value: Int) = Progn(SetToZero, repeat(Incr, value))

  // Problem 179
  val sourceCode = Progn(
    Seek(3),
    Incr, Incr, Incr,
    setTo(72),
    Write,
  )

}

@main def main() = {
  val compiler = Snusp.Compiler()
  Snusp.sourceCode.compileTo(compiler)
  println(compiler.outputMap.toCode())
}
