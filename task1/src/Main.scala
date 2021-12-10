import java.io.File

import scala.io.Source

object Main {



  def main(args: Array[String]): Unit = {

    val fileStrings = Source.fromFile("input").getLines

    val (first, second) = fileStrings.map(_.toInt).duplicate

    second.next

    val descendingCount = first.zip(
      second
    ).count {
      case (left, right) =>
        left < right
    }

    println(descendingCount)
  }
}
