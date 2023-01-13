package day15

import helpers.Point
import scala.util.control.Breaks.*

case class SensorReport(val sensor: Point, val beacon: Point):
  def manhattanDistance(): Int = sensor.manhattanDistance(beacon)
end SensorReport

def getPointsAtManhattanDistance(origin: Point, distance: Int): IndexedSeq[Point] = {
  // We're walking the "circle" created by the distance from left to right
  // We walk left to right twice, tracing the top of the circle, then the bottom
  val columnIterator = Range.inclusive(origin.column - distance, origin.column + distance) ++ Range(origin.column - distance + 1, origin.column + distance)
  // Each range for the rows covers 1/4 of the circle
  val rowIterator = Range(origin.row, origin.row + distance) ++ Range.inclusive(origin.row + distance, origin.row, -1)
    ++ Range(origin.row - 1, origin.row - distance, -1) ++ Range(origin.row - distance, origin.row)

  // is seems like there should be a way to do this lazily, like with view or lazyZip
  columnIterator.zip(rowIterator).map((col, row) => Point(row, col))
}

def part1(reports: Array[SensorReport]): Int = {
  val beacons = reports.foldLeft(Set.empty[Point])((s, r) => s + r.beacon)

  reports.foldLeft(Set.empty[Point]) { (sensorPoints, report) =>
    // need the set of points where y (row) = 2000000 that have
    // manhattan distance to sensor point A <= manhattan distance from A's closest beacon to sensor point A
    // |sensor.row - 2000000| + |sensor.column - X| <= SensorReport.manhattanDistance
    // |sensor.column - X| <= SensorReport.manhattanDistance - |sensor.row - 2000000|
    //    SensorReport.manhattanDistance - |sensor.row - 2000000| is a constant, we'll call it Z
    // |sensor.column - X| <= Z
    // -Z <= sensor.column - X <= Z
    //  subtract sensor.column from both sides
    // -Z - sensor.column <= -X <= Z - sensor.column
    //  divide by -1, causing inequalities to switch
    // Z + sensor.column >= X >= -Z + sensor.column
    //  rewrite to make it a little easier to read
    // sensor.column - Z <= X <= sensor.column + Z
    val targetRow = 2000000
    val Z = report.manhattanDistance() - (report.sensor.row - targetRow).abs

    sensorPoints ++ Range.inclusive(report.sensor.column - Z, report.sensor.column + Z)
      .foldLeft(Set.empty[Point])((subset, column) => subset + Point(targetRow, column))
  }
    .filter(p => !beacons.contains(p))
    .size
}

@main def day15(): Unit = {
  val parseExpression = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r

  val reports = io.Source.fromResource("day15.txt").getLines().map { line =>
    line match
      case parseExpression(sensorX, sensorY, beaconX, beaconY) => SensorReport(
        Point(sensorY.toInt, sensorX.toInt),
        Point(beaconY.toInt, beaconX.toInt)
      )
      case _ => throw new Error("Unable to parse line")
  }.toArray

  println(s"Part 1 = ${part1(reports)}")

  val part2upperBound = 4000000
  var distressBeacon: Option[Point] = None
  breakable {
    for (report <- reports) {
      val filtered = getPointsAtManhattanDistance(report.sensor, report.manhattanDistance() + 1)
        .filter(point => point.row >=0 && point.row <= part2upperBound && point.column >= 0 && point.column <= part2upperBound)
        .filter(point => reports.forall(report => report.sensor.manhattanDistance(point) > report.manhattanDistance()))

      if (filtered.length > 0) {
        distressBeacon = Some(filtered.head)
        break
      }
    }
  }

  println(s"Part 2 = ${BigInt(distressBeacon.get.column) * 4000000 + distressBeacon.get.row}")
}
