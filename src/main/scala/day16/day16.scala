package day16

case class Valve(name: String, rate: Int, connectedTo: Set[String])

case class ValveState(valve: Valve, isOpen: Boolean):
  def isClosed: Boolean = !isOpen
end ValveState

case class ValvePair(a: String, b: String)

class FloydWarshallDistances:
  var distances = Map.empty[ValvePair, Int]

  def add(pair: ValvePair, distance: Int): Unit = {
    distances = distances + (pair -> distance)
  }

  def get(pair: ValvePair): Int = {
    // minor hack since we don't have a concept of infinity and using Int.MaxValue is problematic because if we add
    // Int.MaxValue + Int.MaxValue we get integer overflow, which messes up the minimum math
    distances.getOrElse(pair, Int.MaxValue / 2)
  }

  override def toString: String = distances.mkString(", ")

end FloydWarshallDistances

def floydWarshall(valves: Map[String, Valve]): FloydWarshallDistances = {
  val distances = FloydWarshallDistances()

  valves.values.foreach { valve =>
    distances.add(ValvePair(valve.name, valve.name), 0) // self
    valve.connectedTo.foreach(connectedValveName => distances.add(ValvePair(valve.name, connectedValveName), 1)) // neighbors
  }

  valves.keys.foreach { k =>
    valves.keys.foreach { i =>
      valves.keys.foreach { j =>
        val existing = distances.get(ValvePair(i, j))
        val potentialNew = distances.get(ValvePair(i, k)) + distances.get(ValvePair(k, j))
        distances.add(ValvePair(i, j), existing.min(potentialNew))
      }
    }
  }

  distances
}

// valves with rate == 0 are excluded from valve states so we can iterate over a smaller collection here
def depthFirstSearch(originalValveStates: scala.collection.mutable.Map[String, ValveState], distances: FloydWarshallDistances, currentValve: Valve, currentTime: Int = 0, totalReleasedPressure: Int = 0, maxSeenSoFar: Int = 0): Int = {
  val timeLimit = 30

  val valveStates = originalValveStates.clone()

  val closedValves = valveStates.values.filter(_.isClosed).map(_.valve)
  val maxAvailable = closedValves.map(_.rate * (timeLimit - currentTime)).sum

  if (currentTime > timeLimit || totalReleasedPressure + maxAvailable < maxSeenSoFar) {
    totalReleasedPressure
  } else {
    // even though it doesn't make sense to do this for the starting valve, it doesn't really matter
    valveStates.update(currentValve.name, ValveState(currentValve, true))

    // we can precalculate the impact of opening this valve
    val newTotalReleasedPressure = totalReleasedPressure + currentValve.rate * (timeLimit - currentTime)

    val candidateValves = closedValves.filter(_ != currentValve)

    if (candidateValves.isEmpty) {
      newTotalReleasedPressure
    } else {
      candidateValves.filter(_ != currentValve).map { closedValve =>
        // add one to account for the time to open the valve we go to, since we only go to valves to open them
        val movementTime = distances.get(ValvePair(currentValve.name, closedValve.name)) + 1

        depthFirstSearch(
          valveStates,
          distances,
          closedValve,
          currentTime + movementTime,
          newTotalReleasedPressure,
          newTotalReleasedPressure.max(maxSeenSoFar)
        )
      }
      .max
    }

  }
}

@main def day16(): Unit = {
  val parseExpression = "Valve ([A-Z][A-Z]) has flow rate=(\\d+); (?:tunnel leads to valve|tunnels lead to valves) ([A-Z][A-Z](?:, [A-Z][A-Z])*)".r

  val valveMap = io.Source.fromResource("day16.txt").getLines().foldLeft(Map.empty[String, Valve]) { (mapPart, line) =>
    line match
      case parseExpression(valveName, rate, toValves) =>
        mapPart + (valveName -> Valve(valveName, rate.toInt, toValves.split(", ").toSet))
      case _ => throw new Error("Unable to parse line")
  }

  val valveStates = valveMap.foldLeft(collection.mutable.Map.empty[String, ValveState]) { (mapPart, tuple) =>
    val (name, valve) = tuple
    if (valve.rate == 0) then mapPart else mapPart + (name -> ValveState(valve, false))
  }

  println(depthFirstSearch(valveStates, floydWarshall(valveMap), valveMap("AA")))
}
