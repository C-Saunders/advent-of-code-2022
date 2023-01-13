package day07

import scala.util.matching.Regex

class File(val size: Int, val name: String)

class Directory(val name: String, val parent: Option[Directory] = None):
  private var files: Set[File] = Set()
  private var children: Set[Directory] = Set()

  def addFile(f: File): Unit = files = files + f

  def addChild(d: Directory): Unit = children = children + d

  def size: Int = files.map(_.size).sum + children.map(_.size).sum

  def path: String = parent.map(_.path).getOrElse("") + name
end Directory

@main def day7: Unit = {
  // we don't care about ls calls, so just match on cd calls
  val cdPattern = "^\\$ cd (.+)$".r
  val dirRegex = "^dir (.*)$".r
  val fileRegex = "^(\\d+) (.*)$".r

  val directories = collection.mutable.Map.empty[String, Directory]

  val rootDirectoryName = "root"
  var workingDirectory = Directory(rootDirectoryName)

  // track the pwd and update when the cd pattern is encountered
  io.Source.fromResource("day07.txt").getLines().foreach(line => {
    line match
      case cdPattern(target) => {
        if (target == "..") {
          workingDirectory = directories(workingDirectory.parent.get.path)
        } else {
          val potentialNewDir = Directory(target, Some(workingDirectory))
          workingDirectory = directories.getOrElseUpdate(potentialNewDir.path, potentialNewDir)
        }
      }
      case dirRegex(name) => {
        val potentialNewDir = Directory(name, Some(workingDirectory))
        val child = directories.getOrElseUpdate(potentialNewDir.path, potentialNewDir)
        workingDirectory.addChild(child)
      }
      case fileRegex(size, name) => {
        workingDirectory.addFile(File(size.toInt, name))
      }
      case _ => ()
  })

  // calculate sizes - it'd be better to memoize, but let's see if it works like this
  val directorySizes = directories.map[String, Int] { case (path, dir) => (path, dir.size) }
    .map((_, size) => size)

  println(s"Part 1 = ${directorySizes.filter(_ <= 100_000).sum}")

  val totalUsed = directorySizes.max
  val totalFree = 70_000_000 - totalUsed
  val minimumAmountToDelete = 30_000_000 - totalFree

  // we could do this in O(n) by traversing the list and tracking the best candidate
  // this this is simpler and works fine for this dataset
  println(s"Part 2 = ${directorySizes.filter(_ >= minimumAmountToDelete).min}")
}
