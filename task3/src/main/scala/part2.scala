import scala.io.Source

object part2 {


  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("example")

    val iter = src.getLines()

    val (head, loop1) = iter.duplicate
    val (lines, loop2) = loop1.duplicate

    val len = head.next.length

    val (gamma, eps) = frequencyPatterns(lines, len)


    val g = Integer.parseInt(gamma.mkString, 2)
    val e = Integer.parseInt(eps.mkString, 2)
    println(s"${gamma.mkString} $g")
    println(s"${eps.mkString} $e")
    println(g*e)


    println("---------------part 2-----------------")

    val (itrOnes, itrZeros) = loop2.duplicate

    var itr: Iterator[String] = filterByBit(gamma(0), 0, itrOnes)
    var onesPattern = gamma



    for (idx <- 0 until len) {
      if (itr.size == 1)
        return

      itr = filterByBit(onesPattern(idx), idx, itr)




    }

    var zerosPattern = eps




    println(ox)
    println(co)

    val oxDec  = Integer.parseInt(ox, 2)
    println(oxDec)
    val coDec  = Integer.parseInt(co, 2)
    println(coDec)

    println(oxDec*coDec)

  }

  def filterByBit(bit: Char, bitPos: Int, in: Iterator[String]): Iterator[String] = {
    in.filter { s =>
      s(bitPos) == bit
    }
  }


  def countOnes(counter: Array[Int], bitAndIdx: (Char, Int)) = {
    bitAndIdx match {
      case ('1', i) => counter(i) += 1
      case _ =>
    }
  }

  def frequencyPatterns(itr: Iterator[String], len: Int) = {
    val counter = Array.fill(len)(0)
    var size = 0

    for  (line <- itr) {
      size += 1
      line.zipWithIndex.foreach(countOnes(counter, _))
    }

    val half = size / 2

    val gamma = counter.map { ct =>
      if (ct >= half)
        '1'
      else
        '0'
    }

    val eps = counter.map { ct =>
      if (ct < half)
        '1'
      else
        '0'
    }

    (gamma, eps)
  }

}
