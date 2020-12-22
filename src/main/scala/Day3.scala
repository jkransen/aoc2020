import scala.annotation.tailrec
import scala.io.Source;

case class Map(trees: List[List[Boolean]]) {
  val width: Int = trees.head.length
  val height: Int = trees.length

  def isTree(x: Int, y: Int): Boolean = trees(y)(x)
}

object Day3 extends App {
  val trees = Source.fromResource("day3").getLines()
    .map(line => line.map(_.equals('#')).toList)
    .toList
  val map: Map = Map(trees)

  @tailrec
  def countTrees(x: Int, y: Int, deltaX: Int = 3, deltaY: Int = 1, count: Long = 0): Long = {
    if (y >= map.height) {
      count
    } else {
      val newCount = if (map.isTree(x, y)) count + 1 else count
      countTrees((x + deltaX) % map.width, y + deltaY, deltaX, deltaY, newCount)
    }
  }

  println("Day 3:1")
  println(countTrees(0, 0))

  println("Day 3:2")
  println(
    countTrees(0, 0, 1, 1) *
    countTrees(0, 0, 3, 1) *
    countTrees(0, 0, 5, 1) *
    countTrees(0, 0, 7, 1) *
    countTrees(0, 0, 1, 2)
  )
}
