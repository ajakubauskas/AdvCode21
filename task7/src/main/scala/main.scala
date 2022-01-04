import scala.io.Source

object main {

  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("input")

    val str = src.getLines().next()

    val crabs = str.split(",").map(_.toInt)

    val min = crabs.min
    val max = crabs.max

    val distances = Array.fill(max - min + 1)(0)

    for (i <- distances.indices) {
      val offset = i + min

      distances(i) = crabs.map(c => math.abs(c - offset)).sum
    }

    val minDist = distances.min

    println(minDist)


  }

}
