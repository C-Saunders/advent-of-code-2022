package day08

import helpers.Point

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.*

class GridIterator(slowRange: Range, fastRange: Range, pointFactory: (slowValue: Int, fastValue: Int) => Point) extends Iterator[Point]:
  private val _fastIteratorValues = fastRange
  private var _fastIterator = _fastIteratorValues.iterator
  private val _slowIterator = slowRange.iterator
  private var _slowIteratorValue = _slowIterator.next()
  var isAtStartOfRowOrColumn = true

  override def hasNext: Boolean = _slowIterator.hasNext || (!_slowIterator.hasNext && _fastIterator.hasNext)

  override def next(): Point = {
    if (!_fastIterator.hasNext) { // we're at the end of a row/col
      _fastIterator = _fastIteratorValues.iterator
      _slowIteratorValue = _slowIterator.next()
      isAtStartOfRowOrColumn = true
    } else {
      isAtStartOfRowOrColumn = false
    }
    pointFactory(_slowIteratorValue, _fastIterator.next())
  }

end GridIterator

class FixedIterator(value: Int) extends Iterator[Int]:
  override def hasNext: Boolean = true

  override def next(): Int = value
end FixedIterator

class Grid(data: ArrayBuffer[IndexedSeq[Int]]):
  private val numRows = data.length
  private val numCols = data(0).length

  def get(point: Point): Int = data(point.row)(point.column)

  // Full grid walk iterators
  // leftToRight starts at the top left corner (0, 0) and goes across each row left to right, like reading English
  def leftToRightIterator: GridIterator = GridIterator(Range(0, numRows), Range(0, numCols), (row, col) => Point(row, col))

  // rightToLeft starts at the top right corner (0, numCols) and goes across each row right to left, like reading Hebrew
  def rightToLeftIterator: GridIterator = GridIterator(Range(0, numRows), Range(0, numCols).reverse, (row, col) => Point(row, col))

  // topToBottom starts at the top left corner (0, 0) and goes down each column
  def topToBottomIterator: GridIterator = GridIterator(Range(0, numCols), Range(0, numRows), (col, row) => Point(row, col))

  // bottomToTop starts at the bottom left corner (numRows, 0) and goes up each column
  def bottomToTopIterator: GridIterator = GridIterator(Range(0, numCols), Range(0, numRows).reverse, (col, row) => Point(row, col))

  // Cardinal direction from starting point iterators
  // row goes backwards from Point.row-1 to 0, col = Point.column
  def northIterator(start: Point): Iterator[Point] = Range(0, start.row).reverse.iterator.zip(FixedIterator(start.column)).map((row, col) => Point(row, col))

  // row goes from Point.row+1 to numRows-1, col = Point.column
  def southIterator(start: Point): Iterator[Point] = Range(start.row + 1, numRows).iterator.zip(FixedIterator(start.column)).map((row, col) => Point(row, col))

  // row = Point.row, col goes from Point.column + 1 to numColumns - 1
  def eastIterator(start: Point): Iterator[Point] = Range(start.column + 1, numCols).iterator.zip(FixedIterator(start.row)).map((col, row) => Point(row, col))

  // row = Point.row, col goes backwards from Point.column - 1 to 0
  def westIterator(start: Point): Iterator[Point] = Range(0, start.column).reverse.iterator.zip(FixedIterator(start.row)).map((col, row) => Point(row, col))

end Grid

def getPointsVisibleFromOutsideGrid(grid: Grid, iterator: GridIterator): Set[Point] = {
  val firstPoint = iterator.next()
  var visiblePoints = Set(firstPoint) // the outermost point is always visible from outside the grid

  var highest = grid.get(firstPoint)
  for (point <- iterator) {
    val current = grid.get(point)
    if (iterator.isAtStartOfRowOrColumn) {
      highest = current
      visiblePoints = visiblePoints + point
    }

    if (current > highest) {
      highest = current
      visiblePoints = visiblePoints + point
    }
    // could break here if highest == 9
  }

  visiblePoints
}

def getPointsVisibleFromInsideGrid(grid: Grid, point: Point, directionIterator: Iterator[Point]): ArrayBuffer[Point] = {
  val visiblePoints: ArrayBuffer[Point] = ArrayBuffer.empty
  val pointHeight = grid.get(point)

  breakable {
    for (lookingAt <- directionIterator) {
      val lookingAtHeight = grid.get(lookingAt)
      visiblePoints.addOne(lookingAt)
      if (lookingAtHeight >= pointHeight) {
        break
      }
    }
  }

  visiblePoints
}

@main def day8(): Unit = {
  val grid = Grid(io.Source.fromResource("day08.txt").getLines()
    .foldLeft(ArrayBuffer.empty[IndexedSeq[Int]])((rows, line) =>
      rows.addOne(line.split("").map(_.toInt))
    )
  )

  val visiblePoints = getPointsVisibleFromOutsideGrid(grid, grid.leftToRightIterator)
    ++ getPointsVisibleFromOutsideGrid(grid, grid.rightToLeftIterator)
    ++ getPointsVisibleFromOutsideGrid(grid, grid.topToBottomIterator)
    ++ getPointsVisibleFromOutsideGrid(grid, grid.bottomToTopIterator)

  println(s"Part 1 = ${visiblePoints.size}")

  val part2 = grid.leftToRightIterator.map(currentPoint => {
    getPointsVisibleFromInsideGrid(grid, currentPoint, grid.northIterator(currentPoint)).length
    * getPointsVisibleFromInsideGrid(grid, currentPoint, grid.southIterator(currentPoint)).length
    * getPointsVisibleFromInsideGrid(grid, currentPoint, grid.eastIterator(currentPoint)).length
    * getPointsVisibleFromInsideGrid(grid, currentPoint, grid.westIterator(currentPoint)).length
  }).max

  println(s"Part 2 = ${part2}")
}
