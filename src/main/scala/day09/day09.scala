package day09

import helpers.Point

enum Direction(val from: String):
  case U extends Direction("U")
  case D extends Direction("D")
  case L extends Direction("L")
  case R extends Direction("R")

case class Move(direction: Direction, amount: Int)
case class SimulationState(positions: List[Point], tailPoints: Set[Point])

def processMove(positions: List[Point], move: Move, tailPoints: Set[Point] = Set.empty): SimulationState = {
  var updatedPositions = positions
  var currentTailPoints = tailPoints + positions.last

  for (_ <- Range(0, move.amount)) {
    val currentHead = updatedPositions.head
    val updatedHead = move.direction match {
      case Direction.U => Point(currentHead.row + 1, currentHead.column)
      case Direction.D => Point(currentHead.row - 1, currentHead.column)
      case Direction.L => Point(currentHead.row, currentHead.column - 1)
      case Direction.R => Point(currentHead.row, currentHead.column + 1)
    }
    updatedPositions = updatedHead :: updatedPositions.tail
    
    updatedPositions = updatedPositions.drop(1).foldLeft(List(updatedHead)) { (list, follower) =>
      val leader = list.last

      val updatedFollower = follower.chebyshevDistance(leader) match {
        case 0 | 1 => follower
        case 2 => {
          val row = if (leader.row > follower.row) {
            follower.row + 1
          } else if (leader.row < follower.row) {
            follower.row - 1
          } else {
            follower.row
          }
          val column = if (leader.column > follower.column) {
            follower.column + 1
          } else if (leader.column < follower.column) {
            follower.column - 1
          } else {
            follower.column
          }

          Point(row, column)
        }
        case other => throw new Error(s"Chebyshev distance ${other} outside expected bounds")
      }

      list :+ updatedFollower
    }

    currentTailPoints = currentTailPoints + updatedPositions.last
  }
  SimulationState(updatedPositions, currentTailPoints)
}

@main def day9(): Unit = {
  val part1 = io.Source.fromResource("day09.txt").getLines()
    .map(line =>
      val split = line.split(" ")
      Move(Direction.valueOf(split.head), split.last.toInt)
    )
    .foldLeft(SimulationState(List(Point(0,0), Point(0,0)), Set.empty))((state, move) =>
      processMove(state.positions, move, state.tailPoints)
    )

  println(s"Part 1 = ${part1.tailPoints.size}")

  val part2 = io.Source.fromResource("day09.txt").getLines()
    .map(line =>
      val split = line.split(" ")
      Move(Direction.valueOf(split.head), split.last.toInt)
    )
    .foldLeft(SimulationState(List(
        Point(0,0),
        Point(0,0),
        Point(0,0),
        Point(0,0),
        Point(0,0),
        Point(0,0),
        Point(0,0),
        Point(0,0),
        Point(0,0),
        Point(0,0),
    ), Set.empty))((state, move) =>
      processMove(state.positions, move, state.tailPoints)
    )

  println(s"Part 2 = ${part2.tailPoints.size}")
}
