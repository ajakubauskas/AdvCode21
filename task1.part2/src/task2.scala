import scala.io.Source

object Task2 {


  def main(args: Array[String]): Unit = {

    val fileStrings = Source.fromFile("input.txt").getLines.map(_.toInt)



    val (a, second) = fileStrings.duplicate
    second.next

    val (first, third) = a.duplicate
    third.next
    third.next


    var prevSum = 99999

    var sumsum = 0

    while (third.hasNext) {

      val sum = first.next() + second.next + third.next()

      if (sum > prevSum)
        sumsum+=1

      prevSum = sum
    }



    println(sumsum)
  }

}
