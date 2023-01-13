package day13

def analyzeLists(left: PacketList, right: PacketList): List[Option[Boolean]] = {
  val leftChildren = left.getChildren().map(Some(_))
  val rightChildren = right.getChildren().map(Some(_))
  
  /* There are scenarios where we can't make a decision about whether the PacketLists are in the correct order based on
    a given item (ints that are equal or empty sub-lists, or sub-lists containing only ints that are equal). Therefore,
    we build a list of the results of analyzing each item, then in listsAreInCorrectOrder we return the first non-None.
  
    In theory we could stop iterating once we have a non-None, but it seems a bit clunky to make that happen.
  
    Also, this means there are situations where listsAreInCorrectOrder throws NoSuchElementException, and we're going to
    let that happen because it shouldn't, based on the input/problem description.
  */
  leftChildren.zipAll(rightChildren, None, None).flatMap { (leftChild, rightChild) =>
    if (leftChild.isEmpty) {
      // If the left list runs out of items first, the inputs are in the right order.
      // If the lists are the same length and no comparison makes a decision about the order,
      // continue checking the next part of the input.
      List(Some(rightChild.isDefined))
    } else if (rightChild.isEmpty) {
      // If the right list runs out of items first, the inputs are not in the right order.
      List(Some(false))
    } else {
      val leftItem = leftChild.get
      val rightItem = rightChild.get

      leftItem match
        case leftList: PacketList =>
          rightItem match
            // If both values are lists, compare the first value of each list, then the second value, and so on.
            case rightList: PacketList => {
              if (leftList == rightList) { // this handles the empty list case and other exact matches
                List(Some(true))
              } else {
                analyzeLists(leftList, rightList)
              }
            }
            // If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison.
            case rightInt: Int => analyzeLists(leftList, PacketList(withChild = Some(rightInt)))
        case leftInt: Int =>
          rightItem match
            // If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison.
            case rightList: PacketList => analyzeLists(PacketList(withChild = Some(leftInt)), rightList)
            // If the left integer is lower than the right integer, the inputs are in the right order.
            // If the left integer is higher than the right integer, the inputs are not in the right order.
            // Otherwise, the inputs are the same integer; continue checking the next part of the input
            case rightInt: Int =>
              if (leftInt < rightInt) {
                List(Some(true))
              } else if (leftInt > rightInt) {
                List(Some(false))
              } else {
                List(None)
              }
    }
  }
}

def listsAreInCorrectOrder(left: PacketList, right: PacketList): Boolean = {
  analyzeLists(left, right).find(_.isDefined).get.get
}

