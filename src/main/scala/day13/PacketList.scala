package day13

import scala.collection.mutable.ArrayBuffer

type PacketElement = PacketList | Int

class PacketList(val parent: Option[PacketList] = None, withChild: Option[PacketElement] = None) {
  private var children = if withChild.isDefined then ArrayBuffer(withChild.get) else ArrayBuffer.empty[PacketElement]

  def addChild(e: PacketElement): PacketList = {
    children.addOne(e)
    this
  }

  def addChildren(items: List[PacketElement]): PacketList = {
    children.addAll(items)
    this
  }
  def getChildren(): List[PacketElement] = children.toList

  override def toString: String = children.toString
}
