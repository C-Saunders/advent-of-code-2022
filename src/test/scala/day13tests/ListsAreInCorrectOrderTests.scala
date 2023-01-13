import day13._

class ListsAreInCorrectOrderTests extends munit.FunSuite {
  test("simple nesting - not well defined") {
    intercept[java.util.NoSuchElementException](listsAreInCorrectOrder(
      parse("[[1]]"),
      parse("[[1]]"))
    )
  }

  test("example pair 1 - left int < right int") {
    assertEquals(listsAreInCorrectOrder(
      PacketList().addChildren(List(1, 1, 3, 1, 1)),
      PacketList().addChildren(List(1, 1, 5, 1, 1))),
      true
    )
  }

  test("modified example pair 1 - left int > right int") {
    assertEquals(listsAreInCorrectOrder(
      PacketList().addChildren(List(1, 1, 6, 1, 1)),
      PacketList().addChildren(List(1, 1, 5, 1, 1))),
      false
    )
  }

  test("example pair 2 - wrap right value then left < right") {
    // [[1],[2,3,4]] vs [[1],4]
    assertEquals(listsAreInCorrectOrder(
      PacketList().addChildren(List(
        PacketList(withChild = Some(1)),
        PacketList().addChildren(List(2, 3, 4))
      )),
      PacketList().addChildren(List(
        PacketList(withChild = Some(1)),
        4
      )),
    ),
      true
    )
  }

  test("modified example pair 2 - wrap right value then left > right") {
    assertEquals(listsAreInCorrectOrder(
      parse("[[1],[2,3,4]]"),
      parse("[[1],1]")
    ),
      false
    )
  }

  test("example pair 3") {
    // [9] vs [[8,7,6]]
    assertEquals(listsAreInCorrectOrder(
      PacketList(withChild = Some(9)),
      PacketList(withChild = Some(
        PacketList().addChildren((List(8, 7, 6)))
      ))
    ),
      false
    )
  }

  test("example pair 4") {
    // [[4,4],4,4] vs [[4,4],4,4,4]
    assertEquals(listsAreInCorrectOrder(
      PacketList().addChildren(List(
        PacketList().addChildren(List(4, 4)), 4, 4
      )),
      PacketList().addChildren(List(
        PacketList().addChildren(List(4, 4)), 4, 4, 4
      )),
    ),
      true
    )
  }

  test("example pair 5") {
    // [7,7,7,7] vs [7,7,7]
    assertEquals(listsAreInCorrectOrder(
      PacketList().addChildren(List(7, 7, 7, 7)),
      PacketList().addChildren(List(7, 7, 7)),
    ),
      false
    )
  }

  test("example pair 6") {
    // [] vs [3]
    assertEquals(listsAreInCorrectOrder(
      PacketList(),
      PacketList(withChild = Some(3)),
    ),
      true
    )
  }

  test("example pair 7") {
    // [[[]]] vs [[]]
    assertEquals(listsAreInCorrectOrder(
      PacketList(withChild = Some(PacketList(withChild = Some(PacketList())))),
      PacketList(withChild = Some(PacketList())),
    ),
      false
    )
  }

  // I didn't feel like manually constructing these, so using parse here
  test("example pair 8") {
    // [1,[2,[3,[4,[5,6,7]]]],8,9] vs [1,[2,[3,[4,[5,6,0]]]],8,9]
    assertEquals(listsAreInCorrectOrder(
      parse("[1,[2,[3,[4,[5,6,7]]]],8,9]"),
      parse("[1,[2,[3,[4,[5,6,0]]]],8,9]"),
    ),
      false
    )
  }

}
