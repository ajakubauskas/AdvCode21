
import scala.io.Source

object part1 {


  def main(args: Array[String]): Unit = {


    val src = Source.fromFile("example")

    val iter = src.getLines()

    val (head, lines) = iter.duplicate

    var size = 0

    val counter = Array.fill(head.next.length)(0)

    for  (line <- lines) {
      size += 1
      line.zipWithIndex.foreach(count(counter, _))
    }

    val half = size / 2

    val gamma = counter.map { ct =>
      if (ct > half)
        '1'
      else
        '0'
    }

    val eps = counter.map { ct =>
      if (ct > half)
        '0'
      else
        '1'
    }

    println(gamma.mkString)
    println(eps.mkString)
    val g = Integer.parseInt(gamma.mkString, 2)
    val e = Integer.parseInt(eps.mkString, 2)
    println(g)
    println(e)

    println(g*e)
  }

  def count(counter: Array[Int], bitAndIdx: (Char, Int)) = {
    bitAndIdx match {
      case ('1', i) => counter(i) += 1
      case _ =>
    }
  }
}
