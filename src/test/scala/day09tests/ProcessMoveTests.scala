package day09tests

import helpers.Point
import day09._

class ProcessMoveTests extends munit.FunSuite {
  test("overlap") {
    val result = processMove(List(Point(1, 0), Point(0, 0)), Move(Direction.D, 1))
    assertEquals[Point, Point](result.positions.head, Point(0, 0))
    assertEquals[Point, Point](result.positions.last, Point(0, 0))
    assertEquals[Set[Point], Set[Point]](result.tailPoints, Set(Point(0, 0)))
  }

  test("1 space apart") {
    val result = processMove(List(Point(0, 0), Point(0, 0)), Move(Direction.D, 1))
    assertEquals[Point, Point](result.positions.head, Point(-1, 0))
    assertEquals[Point, Point](result.positions.last, Point(0, 0))
    assertEquals[Set[Point], Set[Point]](result.tailPoints, Set(Point(0, 0)))
  }

  test("2 spaces apart - up") {
    val result = processMove(List(Point(1, 0), Point(0, 0)), Move(Direction.U, 1))
    assertEquals[Point, Point](result.positions.head, Point(2, 0))
    assertEquals[Point, Point](result.positions.last, Point(1, 0))
    assertEquals[Set[Point], Set[Point]](result.tailPoints, Set(Point(0, 0), Point(1, 0)))
  }

  test("2 spaces apart - down") {
    val result = processMove(List(Point(0, 0), Point(0, 0)), Move(Direction.D, 2))
    assertEquals[Point, Point](result.positions.head, Point(-2, 0))
    assertEquals[Point, Point](result.positions.last, Point(-1, 0))
    assertEquals[Set[Point], Set[Point]](result.tailPoints, Set(Point(0, 0), Point(-1, 0)))
  }

  test("2 spaces apart - left") {
    val result = processMove(List(Point(0, 0), Point(0, 0)), Move(Direction.L, 2))
    assertEquals[Point, Point](result.positions.head, Point(0, -2))
    assertEquals[Point, Point](result.positions.last, Point(0, -1))
    assertEquals[Set[Point], Set[Point]](result.tailPoints, Set(Point(0, 0), Point(0, -1)))
  }

  test("2 spaces apart - right") {
    val result = processMove(List(Point(0, 0), Point(0, 0)), Move(Direction.R, 2))
    assertEquals[Point, Point](result.positions.head, Point(0, 2))
    assertEquals[Point, Point](result.positions.last, Point(0, 1))
    assertEquals[Set[Point], Set[Point]](result.tailPoints, Set(Point(0, 0), Point(0, 1)))
  }

  test("diagonal - one step") {
    val result = processMove(List(Point(1, 1), Point(0, 0)), Move(Direction.U, 1))
    assertEquals[Point, Point](result.positions.head, Point(2, 1))
    assertEquals[Point, Point](result.positions.last, Point(1, 1))
    assertEquals[Set[Point], Set[Point]](result.tailPoints, Set(Point(0, 0), Point(1, 1)))
  }

  test("diagonal - two steps") {
    val result = processMove(List(Point(1, 1), Point(0, 0)), Move(Direction.R, 2))
    assertEquals[Point, Point](result.positions.head, Point(1, 3))
    assertEquals[Point, Point](result.positions.last, Point(1, 2))
    assertEquals[Set[Point], Set[Point]](result.tailPoints, Set(Point(0, 0), Point(1, 1), Point(1, 2)))
  }

  test("three nodes - single movement direction") {
    val result = processMove(List(
      Point(0, 0),
      Point(0, 0),
      Point(0, 0),
    ), Move(Direction.R, 4))
    assertEquals[Point, Point](result.positions.head, Point(0, 4))
    assertEquals[Point, Point](result.positions.last, Point(0, 2))
    assertEquals[Set[Point], Set[Point]](result.tailPoints, Set(
      Point(0, 0),
      Point(0, 1),
      Point(0, 2))
    )
  }

  test("more nodes - more complicated") {
    val firstMoveResult = processMove(List(
      Point(0, 0),
      Point(0, 0),
      Point(0, 0),
      Point(0, 0),
    ), Move(Direction.R, 4))
    val secondMoveResult = processMove(firstMoveResult.positions, Move(Direction.U, 4), firstMoveResult.tailPoints)
    assertEquals[Point, Point](secondMoveResult.positions.head, Point(4, 4))
    assertEquals[Point, Point](secondMoveResult.positions.last, Point(2, 3))
    assertEquals[Set[Point], Set[Point]](secondMoveResult.tailPoints, Set(
      Point(0, 0),
      Point(0, 1),
      Point(1, 2),
      Point(2, 3),
    ))
  }
}
