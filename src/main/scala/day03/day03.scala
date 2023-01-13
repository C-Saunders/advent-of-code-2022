package day03

def charToPriority(char: Char): Int = {
  if (char.isLower) {
      char.toInt - ('a'.toInt - 1)
    } else {
      char.toInt - ('A'.toInt - 1) + 26
    }
}

@main def day3: Unit = {
  val part1 = io.Source.fromResource("day03.txt").getLines()
    .map { line => line.splitAt(line.length() / 2) }
    .map { case (left, right) => left.toSet.intersect(right.toSet).head }
    .map { charToPriority }
    .sum

  println(s"Part 1 = ${part1}")

  val part2 = io.Source.fromResource("day03.txt").getLines()
    .grouped(3)
    .map { group => group.map(_.toSet).reduce{ (left, right) => left.intersect(right) }.head }
    .map { charToPriority }
    .sum

  println(s"Part 2 = ${part2}")
}
