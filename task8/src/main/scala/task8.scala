import scala.io.Source

object task8 {

  def main(args: Array[String]) : Unit = {

    val src = Source.fromFile("input")

    def knowns(s: String) =
      s.length == 2 || s.length == 4 || s.length == 3 || s.length == 7

    val rhsSegments = src.getLines().map(_.split('|')).map(_.tail).flatMap(_.map(_.split(" ")))

    println(
      rhsSegments.map(_.count(knowns)).sum
    )


  }

}
