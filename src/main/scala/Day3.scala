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

  @scala.annotation.tailrec
  def countTrees(x: Int, y: Int, deltaX: Int, deltaY: Int, count: Long = 0): Long = {
    if (y >= map.height) {
      count
    } else {
      val newCount = if (map.isTree(x, y)) count + 1 else count
      countTrees((x + deltaX) % map.width, y + deltaY, deltaX, deltaY, newCount)
    }
  }

  println(
    countTrees(0,0, 1, 1) *
    countTrees(0,0, 3, 1) *
    countTrees(0,0, 5, 1) *
    countTrees(0,0, 7, 1) *
    countTrees(0,0, 1, 2)
  )
}
