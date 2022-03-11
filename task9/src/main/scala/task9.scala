import scala.io.Source

object task9 {

  def main(args: Array[String]) : Unit = {

    val src = Source.fromFile("example.txt")

    val lines = src.getLines().toArray.map { line =>
      line.map(_.asDigit).toArray
    }

    val hm = new HeightMap(lines)

    lines.head.indices.foreach { x =>
      lines.indices.foreach { y =>
        if (hm.isLowest(x, y))
          println(lines(y)(x))
      }

    }




  }

  class HeightMap(lines: Array[Array[Int]]) {
    val width = lines.head.length - 1
    val height = lines.length - 1

    def isLowest(x: Int, y: Int) = {
      val left = lines(y)(x - 1 max 0)
      val right = lines(y)(x + 1 min width)
      val top = lines(y - 1 max 0)(x)
      val bottom = lines(y + 1 min height)(x)

      
      lines(y)(x) < left &&
        lines(y)(x) < right &&
        lines(y)(x) < top &&
        lines(y)(x) < bottom
    }
  }

}
