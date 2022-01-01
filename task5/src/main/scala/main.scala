import scala.io.Source

object main {

  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("example")

  }


  case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
    def coverCoords(x: Int, y: Int) = {
      val inX = x1 <= x && x <= x2 || x2 <= x && x <= x1
      val inY =  y1 <= y && y <= y2 || y2 <= y && y <= y1

      inX && inY
    }
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
