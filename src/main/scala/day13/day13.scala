package day13

import scala.collection.mutable.ArrayBuffer

@main def day13(): Unit = {
  val part1 = io.Source.fromResource("day13.txt").getLines()
    .filter(!_.isBlank)
    .grouped(2)
    .zipWithIndex
    .collect {
      case (pair, index) if listsAreInCorrectOrder(parse(pair.head), parse(pair.last)) => index + 1
    }
    .sum

  println(s"Part 1 = $part1")

  val dividerPackets = List("[[2]]", "[[6]]")
  val part2 = io.Source.fromResource("day13.txt").getLines()
    .filter(!_.isBlank)
    .toArray
    .appendedAll(dividerPackets)
    .sortWith((left, right) => listsAreInCorrectOrder(parse(left), parse(right)))
    .zipWithIndex
    .filter((packet, _) => dividerPackets.contains(packet))
    .map(_._2 + 1)
    .product

  println(s"Part 2 = $part2")
}
