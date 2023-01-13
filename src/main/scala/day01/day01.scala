package day01

import scala.collection.mutable.ArrayBuffer

@main def day1: Unit = {
  val sums = io.Source.fromResource("day01.txt").mkString.split("\n\n").map(group => group.split("\n").map(_.toInt).sum).toSeq

  println(s"Part 1 = ${sums.max}")
  println(s"Part 2 = ${sums.sorted(Ordering.Int.reverse).iterator.take(3).sum}")
}
