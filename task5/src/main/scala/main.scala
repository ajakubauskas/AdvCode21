import scala.io.Source

object main {

  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("example")

    val vents = (
      for {
      lineStr <- src.getLines()
    } yield Line.parse(lineStr)
      ).toList

    val grid = Array.fill(vents.map(_.maxX).max + 1)(Array.fill(vents.map(_.maxY).max + 1)(0))

    for (i <- grid.indices; j <- grid.head.indices) {

      grid(i)(j) = vents.count(_.coverCoords(i, j))
    }

    val g = Grid(grid)

    println(g)


    println(grid.map(_.count(_ >= 2)).sum)

  }

  case class Grid(grid: Array[Array[Int]]) {

    override def toString: String = {

      grid.head.indices.map { j =>
        grid.indices.map { i =>
          if (grid(i)(j) == 0) "." else grid(i)(j).toString
        }.mkString
      }.mkString("\n")

    }
  }


  case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {

    private val onlyHorizontalOrVertical = x1 == x2 || y1 == y2

    private val onlyDiagonal = math.abs(x1 - x2) == math.abs(y1 - y2)

    def coverCoords(x: Int, y: Int) = {
      val inX = x1 <= x && x <= x2 || x2 <= x && x <= x1
      val inY =  y1 <= y && y <= y2 || y2 <= y && y <= y1

      onlyHorizontalOrVertical && inX && inY
    }

    def maxX = math.max(x1, x2)
    def maxY = math.max(y1, y2)
  }

  object Line {
    def parse(s: String): Line = {
      s match {
        case s"$a1,$b1 -> $a2,$b2" =>
          Line(a1.toInt, b1.toInt, a2.toInt, b2.toInt)
      }
    }
  }
}
