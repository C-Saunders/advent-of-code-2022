package helpers

class Point(val row: Int, val column: Int) extends Ordered[Point]:
  override def toString: String = s"(${row}, ${column})"

  override def compare(that: Point): Int = if (row.compareTo(that.row) == 0) column.compareTo(that.column) else row.compareTo(that.row)

  override def equals(o: Any): Boolean = o match {
    case that: Point => row == that.row && column == that.column
    case _ => false
  }
  override def hashCode(): Int = (row, column).hashCode()

  def manhattanDistance(other: Point): Int = (row - other.row).abs + (column - other.column).abs

  def chebyshevDistance(other: Point): Int = List((row - other.row).abs, (column - other.column).abs).max
  
  // this doesn't assume rows and cols can't be negative and it doesn't know about grid bounds
  // it just returns all four points around this one
  def getOrthogonallyAdjacentPoints(): Set[Point] = Set(
    Point(row - 1, column),
    Point(row + 1, column),
    Point(row, column - 1),
    Point(row, column + 1),
  )
end Point
