import scala.io.Source

object part2 {


  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("example")

    val lines = src.getLines().toList


    val len = lines.head.length

    val fGamma = (count: Int, half: Int) =>
      {
        if (count >= half)
          '1'
        else
          '0'
      }
    val gamma = frequencyPatterns(lines.iterator, len)(fGamma)

    val fEps = { (count: Int, half: Int) =>
      if (count < half)
        '1'
      else
        '0'
    }
    val eps = frequencyPatterns(lines.iterator, len)(fEps)


    val g = Integer.parseInt(gamma.mkString, 2)
    val e = Integer.parseInt(eps.mkString, 2)
    println(s"${gamma.mkString} $g")
    println(s"${eps.mkString} $e")
    println(g*e)


    println("---------------part 2-----------------")

    var itr = filterByBit(gamma(0), 0, lines.iterator).toList
    var onesPattern = gamma


    for (idx <- 0 until len) {
      if (itr.size == 1)
        return

      itr = filterByBit(onesPattern(idx), idx, itr.iterator).toList

      println(onesPattern.mkString)
      println(itr)

      onesPattern = frequencyPatterns(itr.iterator, len)(fGamma)

    }

    var zerosPattern = eps


    val ox = itr.head


    println(ox)
//    println(co)

    val oxDec  = Integer.parseInt(ox, 2)
    println(oxDec)
//    val coDec  = Integer.parseInt(co, 2)
//    println(coDec)

//    println(oxDec*coDec)

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

  def frequencyPatterns(itr: Iterator[String], len: Int)(f: (Int, Int) => Char) = {
    val counter = Array.fill(len)(0)
    var size = 0

    for  (line <- itr) {
      size += 1
      line.zipWithIndex.foreach(countOnes(counter, _))
    }

    val half = size / 2

    counter.map(f(_, half))
  }

}
