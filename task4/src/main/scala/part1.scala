import scala.collection.mutable
import scala.io.Source

object part1 {

  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("input.part1")
//    val src = Source.fromFile("example")

    val lines = src.getLines()

    val numbers = lines.next().split(",").map(_.toInt)

    val builder = new mutable.ArrayBuilder.ofRef[BingoBoard]()

    while(lines.hasNext) {
      builder.addOne(BingoBoard.parse(lines))
    }

    val boards = builder.result()



//    // part 1 solution
//    for (n <- numbers) {
//
//      boards.find { board =>
//        board.enterAndCheck(n)
//      }.foreach { board =>
//        println(board.sumUnmarked * n)
//        return
//      }
//    }

    // part 2
    val entriesToWin = boards.map { board =>

      numbers.foldLeft(-1) { case (acc, num) =>
        if (board.isWinner)
          acc
        else {
          board.enter(num)
          acc + 1
        }
      }

    }

    val (lastWinner, idx) = (boards zip entriesToWin).maxBy(_._2)

    println(lastWinner.sumUnmarked * numbers(idx))


  }

  case class BingoBoard(rows: Array[Array[Int]]) {

    private val marked: Array[Array[Boolean]] = Array.fill(5)(Array.fill(5)(false))

    private def winnerRow(markedRows: Array[Array[Boolean]]) = markedRows.exists(_.forall(_ == true))

    def isWinner = winnerRow(marked) || winnerRow(marked.transpose)

    def enter(x: Int): Unit = {
      for (i <- 0 until 5; j <- 0 until 5) {
        if (rows(i)(j) == x) {
            marked(i)(j) = true
            return
          }
      }
    }

    def sumUnmarked = {
      val unmarked = for {
        i <- 0 until 5
        j <- 0 until 5
      } yield {
        if (marked(i)(j))
          0
        else rows(i)(j)
      }
      unmarked.sum
    }

    def enterAndCheck(x: Int): Boolean = {
      enter(x)
      isWinner
    }

    override def toString: String = {
      "--------------\n" +
      rows.map(_.mkString(" ")).mkString("\n") +
        "\n--------------"
    }
  }

  object BingoBoard {
    def parse(strs: Iterator[String]): BingoBoard = {
      strs.next()

      BingoBoard(Array(
        strs.next().split(" ").filter(_.nonEmpty).map(_.toInt),
        strs.next().split(" ").filter(_.nonEmpty).map(_.toInt),
        strs.next().split(" ").filter(_.nonEmpty).map(_.toInt),
        strs.next().split(" ").filter(_.nonEmpty).map(_.toInt),
        strs.next().split(" ").filter(_.nonEmpty).map(_.toInt)
      ))
    }
  }

}
