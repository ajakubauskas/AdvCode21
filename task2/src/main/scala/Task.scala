import scala.io.Source

object Task {

  def main(args: Array[String]): Unit = {


    var xPos = 0

    var yPos = 0

    var aim = 0



    Source.fromFile("input2").getLines.toList.foreach {
      case s"forward $x" =>
        xPos += x.toInt
        yPos += x.toInt * aim
      case s"up $y" => aim -= y.toInt
      case s"down $y" => aim += y.toInt
      case _ =>
    }


    println(xPos*yPos)
  }

}
