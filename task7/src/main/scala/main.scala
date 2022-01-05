import scala.io.Source

object main {

  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("input")

    val str = src.getLines().next()

    val crabs = str.split(",").map(_.toInt)

    val min = crabs.min
    val max = crabs.max

    // part 1

    val distances = Array.fill(max - min + 1)(0)

    for (i <- distances.indices) {
      val offset = i + min

      distances(i) = crabs.map(c => math.abs(c - offset)).sum
    }

    val minDist = distances.min

    println(minDist)

    // part 2
    // Sum 1 .. n is known as Newton's formula: S(n) = n(n+1) / 2
    // however for performance reasons it would be more prudent to precompute a "distance table" -
    // an array of cumulative distances where distTable(i) = i + distTable(i-1) and then do array lookups!

    def newtSum(n: Int) = n*(n+1) / 2

    for (i <- distances.indices) {
      val offset = i + min

      distances(i) = crabs.map(c => newtSum(math.abs(c - offset))).sum
    }


    val newMinDist = distances.min

    println(newMinDist)


  }

}
