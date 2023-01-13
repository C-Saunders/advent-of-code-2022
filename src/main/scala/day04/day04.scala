package day04

@main def day4: Unit = {
  val part1 = io.Source.fromResource("day04.txt").getLines()
  .map { line =>
    val ranges = line.split(",")
      .map { rawRange => 
        val ints = rawRange.split("-").map(_.toInt)
        Range.inclusive(ints.head, ints.last)
      }
    val left = ranges.head
    val right = ranges.last

    if (left.containsSlice(right) || right.containsSlice(left)) {
      1
    } else {
      0
    }
  }
  .sum

  println(s"Part 1 = ${part1}")

  val part2 = io.Source.fromResource("day04.txt").getLines()
  .map { line =>
    val ranges = line.split(",")
      .map { rawRange => 
        val ints = rawRange.split("-").map(_.toInt)
        Range.inclusive(ints.head, ints.last)
      }
    val left = ranges.head
    val right = ranges.last

    if (left.intersect(right).length > 0) {
      1
    } else {
      0
    }
  }
  .sum

  println(s"Part 2 = ${part2}")
}
