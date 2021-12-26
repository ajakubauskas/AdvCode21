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

    val matches = loop2.map { s =>
      (s, countMatches(s, gamma), countMatches(s, eps))
    }.toList


    val ox = matches.maxBy(_._2)._1

    println("===========")
    matches.groupBy(_._2).mapValues(_.map(_._1).map(_.mkString)).foreach { case (k, v) =>
      println(s"$k ${v.mkString(",")}")
    }
    println("===========")

    println(ox)
    val co = matches.maxBy(_._3)._1
    println(co)

    val oxDec  = Integer.parseInt(ox, 2)
    println(oxDec)
    val coDec  = Integer.parseInt(co, 2)
    println(coDec)

    println(oxDec*coDec)

  }

  def countMatches(string: String, pattern: Array[Char]): Int = {
    var count = 0
    val s = string.toCharArray
    pattern.indices.foreach { idx =>
      if (s(idx) == pattern(idx))
        count += 1
      else
        return count

    }
    count
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
