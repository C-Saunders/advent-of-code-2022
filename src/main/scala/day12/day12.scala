package day12

import helpers.Point

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.*

type ShortestPathDistances = Map[Point, Int]

// this was translated fairly directly from https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
def dijkstra(grid: Map[Point, Char], start: Point, end: Point, currentShortestPathLength: Int = Int.MaxValue): ShortestPathDistances = {
  var distances: ShortestPathDistances = Map(start -> 0)
  var shortestPathTree: Map[Point, Option[Point]] = grid.map {
    case (point, _) => (point, None)
  }
  var queue = grid.keySet

  breakable { while(!queue.isEmpty) {
    val filteredDistances = distances
      .filter((point, *) => queue.contains(point))
    val (unvisitedMinPoint, distance) = if (filteredDistances.isEmpty) {
      (queue.head, Int.MaxValue)
    } else {
      filteredDistances.minBy((*, distance) => distance)
    }

    if (unvisitedMinPoint == end) break
    if (distance > currentShortestPathLength) break

    queue = queue - unvisitedMinPoint

    val neighbors = unvisitedMinPoint.getOrthogonallyAdjacentPoints().filter(p => queue.contains(p))
    neighbors.foreach { neighbor =>
      val alternatePathLength = distances.get(unvisitedMinPoint) match
        case Some(distance) => getDistanceToAdjacentPoint(grid, unvisitedMinPoint, neighbor) match {
          case Some(nextStep) => Some(nextStep + distance)
          case None => None
        }
        case None => None

      if (alternatePathLength.isDefined) {
        val value = alternatePathLength.get
        if (value < distances.get(neighbor).getOrElse(Int.MaxValue)) {
          distances = distances.updated(neighbor, value)
          shortestPathTree = shortestPathTree.updated(neighbor, Some(unvisitedMinPoint))
        }
      }
    }
  }}

  distances
}

def getDistanceToAdjacentPoint(grid: Map[Point, Char], currentPoint: Point, adjacentPoint: Point): Option[Int] = {
  val currentChar = grid(currentPoint)
  val maybeAdjacentChar = grid.get(adjacentPoint)

  maybeAdjacentChar match
    case Some(value) => if value <= currentChar + 1 then Some(1) else None
    case None => None
}

@main def day12(): Unit = {
  var start: Option[Point] = None
  var end: Option[Point] = None
  val grid = io.Source.fromResource("day12.txt").getLines().zipWithIndex.foldLeft(Map.empty[Point, Char]) { (grid, lineTuple) =>
    val (line, rowNumber) = lineTuple
    grid ++ line.split("").map(_.head).zipWithIndex.foldLeft(Map.empty[Point, Char]) { (gridLine, charTuple) =>
      val (rawChar, columnNumber) = charTuple
      val point = Point(rowNumber, columnNumber)
      val char = if (rawChar == 'S') {
        start = Some(point)
        'a'
      } else if (rawChar == 'E') {
        end = Some(point)
        'z'
      } else {
        rawChar
      }

      gridLine.updated(point, char)
    }
  }

  val startPoint = start.get
  val endPoint = end.get

  val distancesFromStart = dijkstra(grid, startPoint, endPoint)

  println(s"Part 1 = ${distancesFromStart(endPoint)}")

  // This approach worked, but it took a little while.
  // It could be better to start at the end and find the first/shortest
  // path to an 'a' point, though that requires different getDistanceToAdjacentPoint logic
  // An approach using solved sub-problems (basically a cache/memo) might also work.
  var currentShortestPathLength = Int.MaxValue
  val possibleStartPoints = grid.filter((_, c) => c == 'a').keySet

  for (currStart <- possibleStartPoints) {
    val currDistances = dijkstra(grid, currStart, endPoint, currentShortestPathLength)
    val pathLength = currDistances.get(endPoint)
    currentShortestPathLength = Seq(pathLength.getOrElse(currentShortestPathLength), currentShortestPathLength).min
  }

  println(s"Part 2 = $currentShortestPathLength")

}
