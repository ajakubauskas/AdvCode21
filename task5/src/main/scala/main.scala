import scala.io.Source

object main {

  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("input")

    val vents = (
      for {
      lineStr <- src.getLines()
    } yield Line.parse(lineStr)
      ).toList

    val grid = Array.fill(vents.map(_.maxX).max)(Array.fill(vents.map(_.maxY).max)(0))

    for (i <- grid.indices; j <- grid.head.indices) {

      grid(i)(j) = vents.count(_.coverCoords(i, j))
    }

    val g = Grid(grid)


    println(grid.map(_.count(_ >= 2)).sum)

  }

  case class Grid(grid: Array[Array[Int]]) {


    // not really pretty
    override def toString: String = {
      val s = for {
        i <- grid.indices


      } yield {
        grid.head.indices.map { j =>
          if (grid(i)(j) == 0) "." else grid(i)(j).toString
        }.mkString

      }.mkString("\n")

      s.mkString
    }
  }


  case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
    def coverCoords(x: Int, y: Int) = {
      val inX = x1 <= x && x <= x2 || x2 <= x && x <= x1
      val inY =  y1 <= y && y <= y2 || y2 <= y && y <= y1

      val onlyHorizontalOrVertical = x1 == x2 || y1 == y2

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
