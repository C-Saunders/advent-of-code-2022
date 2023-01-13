package day11

import scala.collection.immutable.SortedMap
import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer

class Monkey(val id: String, startingItems: Array[BigInt], operationSymbol: String, operationQuantity: String, val testDivisor: Int, throwToOnTestTrue: String, throwToOnTestFalse: String):
  private val items = ArrayBuffer.from(startingItems)
  private var numberOfItemsInspected = BigInt(0)

  def incrementNumberOfItemsInspected(): Unit = numberOfItemsInspected = numberOfItemsInspected + 1
  def getNumberOfItemsInspected(): BigInt = numberOfItemsInspected

  def getUpdatedWorryLevel(worryLevel: BigInt): BigInt = {
    // this feels a little sloppy, but it's safe since the input is always an int or "old"
    val quantity = operationQuantity.toIntOption match
      case Some(v) => BigInt(v)
      case None => worryLevel

    operationSymbol match
      case "*" => worryLevel * quantity
      case "+" => worryLevel + quantity
      case _ => throw new Error(s"Unknown worry level update operator $operationSymbol")
  }

  def getThrowToMonkeyId(worryLevel: BigInt): String = if worryLevel % testDivisor == 0 then throwToOnTestTrue else throwToOnTestFalse

  def getCurrentItems() = items.toArray

  def throwItem() = items.remove(0)

  def receiveItem(item: BigInt) = items.addOne(item)

  override def toString: String =
    s"""
      | Monkey $id
      | items ${items.mkString(", ")}
      | numberOfItemsInspected $numberOfItemsInspected
      |""".stripMargin

end Monkey

object Monkey {
  def builder(): MonkeyBuilder = MonkeyBuilder("", Array.empty, "", "", -1, "", "")
}

case class MonkeyBuilder(id: String, startingItems: Array[BigInt], operationSymbol: String, operationQuantity: String, testDivisor: Int, throwToOnTestTrue: String, throwToOnTestFalse: String):
  def withId(id: String): MonkeyBuilder = copy(id = id)
  def withStartingItems(items: Array[BigInt]): MonkeyBuilder = copy(startingItems = items)
  def withOperation(symbol: String, quantity: String): MonkeyBuilder = copy(operationSymbol = symbol, operationQuantity = quantity)
  def withTestDivisor(divisor: Int): MonkeyBuilder = copy(testDivisor = divisor)
  def withThrowToOnTestTrue(throwTo: String): MonkeyBuilder = copy(throwToOnTestTrue = throwTo)
  def withThrowToOnTestFalse(throwTo: String): MonkeyBuilder = copy(throwToOnTestFalse = throwTo)

  def build(): Monkey = Monkey(id, startingItems, operationSymbol, operationQuantity, testDivisor, throwToOnTestTrue, throwToOnTestFalse)
end MonkeyBuilder

def getMonkeys(filename: String): SortedMap[String, Monkey] = {
  val monkeyNumberExpr = "Monkey ([0-7]):".r
  val startingItemsExpr = "Starting items: (.*)".r
  val operationExpr = "Operation: new = old ([*+]) (\\d+|old)".r
  val testExpr = "Test: divisible by (\\d+)".r
  val trueExpr = "If true: throw to monkey ([0-7])".r
  val falseExpr = "If false: throw to monkey ([0-7])".r

  io.Source.fromResource(filename).mkString
    .split("\\n\\n")
    .foldLeft(SortedMap.empty[String, Monkey]) { (map, block) =>
      var monkeyBuilder = Monkey.builder()

      block.split("\\n").foreach { line =>
        line.trim match
          case monkeyNumberExpr(id) => monkeyBuilder = monkeyBuilder.withId(id)
          case startingItemsExpr(items) => monkeyBuilder = monkeyBuilder.withStartingItems(items.split(", ").map(BigInt(_)))
          case operationExpr(symbol, quantity) => monkeyBuilder = monkeyBuilder.withOperation(symbol, quantity)
          case testExpr(divisor) => monkeyBuilder = monkeyBuilder.withTestDivisor(divisor.toInt)
          case trueExpr(id) => monkeyBuilder = monkeyBuilder.withThrowToOnTestTrue(id)
          case falseExpr(id) => monkeyBuilder = monkeyBuilder.withThrowToOnTestFalse(id)
          case _ => throw new Error("Line didn't match any regex")
      }

      val monkey = monkeyBuilder.build()
      map.updated(monkey.id, monkey)
    }
}

def getLevelOfMonkeyBusiness(monkeys: SortedMap[String, Monkey]): BigInt = {
  monkeys.values.toArray
    .sortBy(m => -m.getNumberOfItemsInspected())
    .take(2)
    .map(_.getNumberOfItemsInspected())
    .product
}

@main def day11(): Unit = {
  val inputFile = "day11.txt"

  val partOneMonkeys = getMonkeys(inputFile)
  for (_ <- Range(0, 20)) {
    partOneMonkeys.foreach { (_, currentMonkey) =>
      // repeatedly popping items might be better here
      currentMonkey.getCurrentItems().foreach { item =>
        currentMonkey.incrementNumberOfItemsInspected()
        val updatedItem = currentMonkey.getUpdatedWorryLevel(item) / 3
        val throwTo = currentMonkey.getThrowToMonkeyId(updatedItem)
        currentMonkey.throwItem()
        partOneMonkeys(throwTo).receiveItem(updatedItem)
      }
    }
  }

  println(s"Part 1 = ${getLevelOfMonkeyBusiness(partOneMonkeys)}")

  val partTwoMonkeys = getMonkeys(inputFile)
  val chineseRemainderTheoremDivisor = partTwoMonkeys.values.map(_.testDivisor).product
  for (_ <- Range(0, 10000)) {
    partTwoMonkeys.foreach { (_, currentMonkey) =>
      // repeatedly popping items might be better here
      currentMonkey.getCurrentItems().foreach { item =>
        currentMonkey.incrementNumberOfItemsInspected()
        val updatedItem = currentMonkey.getUpdatedWorryLevel(item) % chineseRemainderTheoremDivisor
        val throwTo = currentMonkey.getThrowToMonkeyId(updatedItem)
        currentMonkey.throwItem()
        partTwoMonkeys(throwTo).receiveItem(updatedItem)
      }
    }
  }

  println(s"Part 2 = ${getLevelOfMonkeyBusiness(partTwoMonkeys)}")

}

