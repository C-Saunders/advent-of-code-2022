package day14

import helpers.Point
import scala.util.control.Breaks.*

def getRocks(filename: String): Set[Point] = {
  io.Source.fromResource(filename).getLines().foldLeft(Set.empty[Point]) { (allRocks, line) =>
    // x,y -> x,y -> ...
    allRocks ++ line.split(" -> ").map { pointStr =>
      val split = pointStr.split(",")
      // x,y where x = distance to right i.e. column and y = distance down i.e. row
      Point(split.last.toInt, split.head.toInt)
    }
      .sliding(2)
      .foldLeft(Set.empty[Point]) { (rockPoints, pointPair) =>
        val start = pointPair.head
        val end = pointPair.last

        val rows = List(start.row, end.row)
        val rowsRange = Range.inclusive(rows.min, rows.max)
        val columns = List(start.column, end.column)
        val columnsRange = Range.inclusive(columns.min, columns.max)

        // pointPairs describe horizontal or vertical "walls", so we can do zipAll with defaults to get all the combos
        rockPoints ++ rowsRange.zipAll(columnsRange, rowsRange.min, columnsRange.min)
          .foldLeft(Set.empty[Point]) { (acc, tuple) =>
            acc + Point(tuple._1, tuple._2)
          }
      }
  }
}

def noRocksBelow(point: Point, rocks: Set[Point]): Boolean = {
  // could build rock map { [column]: rows[] } to speed this up, though that seems a bit silly
  // or find the dimensions of the space...
  !rocks.exists(rock => rock.column == point.column && rock.row > point.row)
}

def part1(rocks: Set[Point]): Int = {
  var occupiedPoints = rocks
  val sandStart = Point(0, 500)
  var sandAt = sandStart
  var sandCount = 0

  breakable {
    while (true) {
      if (!occupiedPoints.contains(Point(sandAt.row + 1, sandAt.column))) {
        sandAt = Point(sandAt.row + 1, sandAt.column)
      } else if (!occupiedPoints.contains(Point(sandAt.row + 1, sandAt.column - 1))) {
        sandAt = Point(sandAt.row + 1, sandAt.column - 1)
      } else if (!occupiedPoints.contains(Point(sandAt.row + 1, sandAt.column + 1))) {
        sandAt = Point(sandAt.row + 1, sandAt.column + 1)
      } else {
        occupiedPoints = occupiedPoints + sandAt
        sandAt = sandStart
        sandCount = sandCount + 1
      }

      if (noRocksBelow(sandAt, rocks)) break
    }
  }

  sandCount
}

def part2(rocks: Set[Point]): Int = {
  var occupiedPoints = rocks
  val sandStart = Point(0, 500)
  var sandAt = sandStart
  var sandCount = 0

  val floorRow = rocks.maxBy(_.row).row + 2

  breakable {
    while (true) {
      if (sandAt.row + 1 == floorRow) {
        occupiedPoints = occupiedPoints + sandAt
        sandAt = sandStart
        sandCount = sandCount + 1
      } else if (!occupiedPoints.contains(Point(sandAt.row + 1, sandAt.column))) {
        sandAt = Point(sandAt.row + 1, sandAt.column)
      } else if (!occupiedPoints.contains(Point(sandAt.row + 1, sandAt.column - 1))) {
        sandAt = Point(sandAt.row + 1, sandAt.column - 1)
      } else if (!occupiedPoints.contains(Point(sandAt.row + 1, sandAt.column + 1))) {
        sandAt = Point(sandAt.row + 1, sandAt.column + 1)
      } else {
        occupiedPoints = occupiedPoints + sandAt
        sandAt = sandStart
        sandCount = sandCount + 1
      }

      if (occupiedPoints.contains(sandStart)) {
        break
      }
    }
  }

  sandCount
}



@main def day14(): Unit = {
  val rocks = getRocks("day14.txt")

  println(s"Part 1  = ${part1(rocks)}")
  println(s"Part 2  = ${part2(rocks)}")

}
