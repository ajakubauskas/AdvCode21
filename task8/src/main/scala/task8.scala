import scala.io.Source

object task8 {

  def main(args: Array[String]) : Unit = {

    val src = Source.fromFile("example")


    // part 1


//    def knowns(s: String) =
//      s.length == 2 || s.length == 4 || s.length == 3 || s.length == 7
//
//    val rhsSegments = src.getLines().map(_.split('|')).map(_.tail).flatMap(_.map(_.split(" ")))
//
//    println(
//      rhsSegments.map(_.count(knowns)).sum
//    )

    // part 2
    for (line <- src.getLines()) {

      val Array(lhs, rhs) = line.split('|').map(_.split(" "))
      println( "signal: " +
        lhs.flatMap(Signal.parse).mkString(" ; ")
      )
      println( "output: " +
        rhs.flatMap(Signal.parse).mkString(" ; ")
      )
    }


  }

}

