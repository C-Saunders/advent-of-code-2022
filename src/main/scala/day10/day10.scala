package day10

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.*

@main def day10(): Unit = {
  var relevantCycleData: Map[Int, Option[Int]] = Map(
    20 -> None,
    60 -> None,
    100 -> None,
    140 -> None,
    180 -> None,
    220 -> None
  )

  val screenRows = ArrayBuffer.fill(6)(ArrayBuffer.empty[Char])
  def getScreenRow(cycleNumber: Int): Int = (cycleNumber - 1) / 40

  def spriteIsVisible(position: Int, cycleNumber: Int): Boolean = {
    val drawingPosition = (cycleNumber - 1) % 40
    position - 1 == drawingPosition ||
      position == drawingPosition ||
      position + 1 == drawingPosition
  }

  var registerValue = 1
  var cycleNumber = 1

  // maybe it would make sense to build either
  // a map of cycle number -> register value
  // an array of register value where the index = cycle number
  // something like this would clean up this mess, I think

  breakable {
    for (line <- io.Source.fromResource("day10.txt").getLines()) {
      if (relevantCycleData.contains(cycleNumber)) {
        relevantCycleData = relevantCycleData.updated(cycleNumber, Some(registerValue * cycleNumber))
      }

      if (spriteIsVisible(registerValue, cycleNumber)) {
        screenRows(getScreenRow(cycleNumber)).addOne('#')
      } else {
        screenRows(getScreenRow(cycleNumber)).addOne(' ')
      }

      cycleNumber = cycleNumber + 1

      val split = line.split(" ")
      val instruction = split(0)
      if (instruction == "addx") {
        if (relevantCycleData.contains(cycleNumber)) {
          relevantCycleData = relevantCycleData.updated(cycleNumber, Some(registerValue * cycleNumber))
        }
        if (spriteIsVisible(registerValue, cycleNumber)) {
          screenRows(getScreenRow(cycleNumber)).addOne('#')
        } else {
          screenRows(getScreenRow(cycleNumber)).addOne(' ')
        }
        cycleNumber = cycleNumber + 1
        registerValue = registerValue + split(1).toInt
      }

      if (cycleNumber > 240) {
        break
      }
    }
  }

  println(s"Part 1 = ${relevantCycleData.values.map(_.get).sum}")

  screenRows.map(_.mkString).foreach(println)
}
