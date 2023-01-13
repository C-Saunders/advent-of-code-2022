import day13._

class ParseTests extends munit.FunSuite  {
  test("empty, 10") {
    assertEquals(parse("[[],[10,3,10,9]]").toString,
      PacketList().addChildren(List(
        PacketList(),
        PacketList().addChildren(List(10, 3, 10, 9))
      )).toString
    )
  }
}
