import scala.io.Source

object main {


  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("input")

    val school = src.getLines().next().split(",").map(Fish.parse).toList

    // part 1
    val after80days = (0 until 80).foldLeft(school) { case (s, _) =>

      s.flatMap(_.next())

    }

    println(after80days.length)

  }


  case class Fish(state: Int) {
    def next(): Seq[Fish] = {
      if (state == 0)
        Fish(6) :: Fish(8) :: Nil
      else
        Fish(state - 1) :: Nil
    }
  }

  object Fish {
    def parse(s: String): Fish = {
      val i  = s.toInt
      assert(i > 0)
      Fish(i)
    }
  }
}
