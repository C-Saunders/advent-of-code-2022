package day05

import scala.util.matching.Regex

class CargoStacks(initialStateLines: Seq[String]):
  val state = initialStateLines.foldLeft(collection.mutable.Map.empty[Int, List[Char]]){
    (map, line) => {
      line.zipWithIndex.foreach {
        case (character, index) => {
          // since we're doing integer division we need to add one and don't need to round
          // for example, the first stack column is at index 2, 2/4 == 0
          val stackIndex = index / 4 + 1
          if (character.isLetter) {
            map.get(stackIndex) match {
              case None => map.put(stackIndex, List(character))
              case Some(stack) => map.put(stackIndex, stack.appended(character))
            }
          }
        }
      }
      map
    }
  }

  private def makeMove(move: CargoMove, reverse: Boolean): Unit = {
    val fromStack = state.get(move.fromStack).get
    val toStack = state.get(move.toStack).get
    state.put(move.fromStack, fromStack.drop(move.count))

    val toAdd: List[Char] = if (reverse) {
      fromStack.take(move.count).reverse
    } else {
      fromStack.take(move.count)
    }
    state.put(move.toStack, toAdd:::toStack)
  }

  def makeOneByOneMove(move: CargoMove): Unit = {
    makeMove(move, true)
  }

  def makeBulkMove(move: CargoMove): Unit = {
    makeMove(move, false)
  }
end CargoStacks

class CargoMove(line: String):
  private val parsePattern: Regex = "^move (\\d+) from (\\d) to (\\d)$".r
  private var _count: Int = 0
  private var _fromStack: Int = 0
  private var _toStack: Int = 0

  line match
    case parsePattern(count, fromStack, toStack) => {
      _count = count.toInt
      _fromStack = fromStack.toInt
      _toStack = toStack.toInt
    }

  def count: Int = _count
  def fromStack: Int = _fromStack
  def toStack: Int = _toStack

  override def toString: String = s"Count = ${_count}, From = ${_fromStack}, To = ${_toStack}"

end CargoMove

@main def day5: Unit = {
  val lines = io.Source.fromResource("day05.txt").getLines().toSeq
  val initialStateLines = lines.takeWhile(!_.isEmpty())
  val instructionLines = lines.dropWhile(!_.isEmpty()).tail // tail to drop leading empty line
  
  val partOneStacks = CargoStacks(initialStateLines)
  instructionLines.map(CargoMove(_)).foreach(partOneStacks.makeOneByOneMove(_))
  println(s"Part 1 = ${partOneStacks.state.valuesIterator.map(_.head).mkString}")

  val partTwoStacks = CargoStacks(initialStateLines)
  instructionLines.map(CargoMove(_)).foreach(partTwoStacks.makeBulkMove(_))
  println(s"Part 2 = ${partTwoStacks.state.valuesIterator.map(_.head).mkString}")
}
