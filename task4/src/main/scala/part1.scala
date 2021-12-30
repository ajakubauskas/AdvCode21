import scala.io.Source

object part1 {

  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("example")

    val lines = src.getLines().toList

  }

  case class BingoBoard(rows: Array[Array[Int]]) {

    private val marked = Array.fill(4)(Array.fill(4)(false))

    def check(): Boolean = ???


    def enter(x: Int) = {
      ???
    }

    def enterAndCheck(x: Int): Boolean = ???
  }

}
