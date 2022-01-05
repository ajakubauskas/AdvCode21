import scala.io.Source

object task8 {

  def main(args: Array[String]) : Unit = {

    val src = Source.fromFile("example")

    def knowns(s: String) =
      s.length == 2 || s.length == 4 || s.length == 3 || s.length == 7


  }

}
