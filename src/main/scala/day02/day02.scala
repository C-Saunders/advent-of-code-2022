package day02

object Throw extends Enumeration {
  type Throw = Value
  val Rock, Paper, Scissors = Value

  def fromString(value: String): Value = value match {
    case "A" => Rock
    case "B" => Paper
    case "C" => Scissors
    case "X" => Rock
    case "Y" => Paper
    case "Z" => Scissors
    case _ => throw new Exception("Oops")
  }

  def getPoints(value: Throw.Value) = value match {
    case Rock => 1
    case Paper => 2
    case Scissors => 3
  }
}

object RoundResult extends Enumeration {
  type RoundResult = Value
  val Win, Loss, Draw = Value

  def fromString(value: String): Value = value match {
    case "X" => Loss
    case "Y" => Draw
    case "Z" => Win
  }

  def getPoints(result: RoundResult.Value) = result match {
    case Win => 6
    case Loss => 0
    case Draw => 3
  }
}

def scoreRoundPart1(theirs: Throw.Value, ours: Throw.Value): Int = {
  val result = ours match {
    case Throw.Rock => theirs match {
      case Throw.Rock => RoundResult.getPoints(RoundResult.Draw)
      case Throw.Paper => RoundResult.getPoints(RoundResult.Loss)
      case Throw.Scissors => RoundResult.getPoints(RoundResult.Win)
    }
    case Throw.Paper => theirs match {
      case Throw.Rock => RoundResult.getPoints(RoundResult.Win)
      case Throw.Paper => RoundResult.getPoints(RoundResult.Draw)
      case Throw.Scissors => RoundResult.getPoints(RoundResult.Loss)
    }
    case Throw.Scissors => theirs match {
      case Throw.Rock => RoundResult.getPoints(RoundResult.Loss)
      case Throw.Paper => RoundResult.getPoints(RoundResult.Win)
      case Throw.Scissors => RoundResult.getPoints(RoundResult.Draw)
    }
  }

  Throw.getPoints(ours)+ result
}

def scoreRoundPart2(theirs: Throw.Value, requiredResult: RoundResult.Value): Int = {
  val ourThrow = theirs match {
    case Throw.Rock => requiredResult match {
      case RoundResult.Win => Throw.Paper
      case RoundResult.Loss => Throw.Scissors
      case RoundResult.Draw => Throw.Rock
    }
    case Throw.Paper => requiredResult match {
      case RoundResult.Win => Throw.Scissors
      case RoundResult.Loss => Throw.Rock
      case RoundResult.Draw => Throw.Paper
    }
    case Throw.Scissors => requiredResult match {
      case RoundResult.Win => Throw.Rock
      case RoundResult.Loss => Throw.Paper
      case RoundResult.Draw => Throw.Scissors
    }
  }

  RoundResult.getPoints(requiredResult) + Throw.getPoints(ourThrow)
}

@main def day2: Unit = {
  val part1 = io.Source.fromResource("day02.txt").getLines()
    .map(line => {
      val pair = line.split(" ")
      scoreRoundPart1(Throw.fromString(pair.head), Throw.fromString(pair.last))
    }).sum
  println(s"Part 1 = ${part1}")
  
  val part2 = io.Source.fromResource("day02.txt").getLines()
    .map(line => {
      val pair = line.split(" ")
      scoreRoundPart2(Throw.fromString(pair.head), RoundResult.fromString(pair.last))
    }).sum
  println(s"Part 2 = ${part2}")
}
