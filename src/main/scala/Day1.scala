import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  val numbers: List[Int] = {
    Source.fromResource("day1").getLines().map(_.toInt).toList
  }

  @tailrec
  def matchPairs(numbers: List[Int], found: List[Int]): List[Int] = {
    if (numbers.isEmpty) {
      found
    } else {
      val first = numbers.head
      val newFound = numbers.tail
        .filter(second => first + second == 2020)
        .map(_ * first)
      matchPairs(numbers.tail, found ++ newFound)
    }
  }

  println("Day 1:1")
  matchPairs(numbers, List())
    .foreach(println)

  val threePairs = for {
    first <- numbers
    second <- numbers
    if (second > first)
    third <- numbers
    if (third > second)
    if (first + second + third == 2020)
  } yield (first * second * third)

  println("Day 1:2")
  threePairs.foreach(println)
}
