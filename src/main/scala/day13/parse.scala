package day13

def parse(line: String): PacketList = {
  var currentList: Option[PacketList] = None

  val chars = line.split("")
  var charIndex = 0
  while (charIndex < chars.length) {
    val char = chars(charIndex)

    if (char == "[") {
      val newList = PacketList(currentList)
      if (currentList.isDefined) {
        currentList.get.addChild(newList)
      }
      currentList = Some(newList)
    } else if (char == "]") currentList = {
      val parent = currentList.get.parent
      if (parent.isDefined) parent
      else currentList
    } else if (char == "1") {
      if (chars(charIndex + 1) == "0") {
        currentList.get.addChild(10)
        charIndex = charIndex + 1
      } else {
        currentList.get.addChild(1)
      }
    } else if (char.toIntOption.isDefined) {
      currentList.get.addChild(char.toInt)
    }

    charIndex = charIndex + 1
  }

  currentList.get
}
