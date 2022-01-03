import scala.io.Source

object main {


  def main(args: Array[String]): Unit = {

    val src = Source.fromFile("input")

    val firstLine = src.getLines().next()

    val school = firstLine.split(",").map(Fish.parse).toList

    // part 1
//    val after80days = (0 until 80).foldLeft(school) { case (s, _) =>
//
//      s.flatMap(_.next())
//
//    }
//
//    println(after80days.length)


    // part 2

    val generations = firstLine.split(",").groupBy(_.toInt).mapValues(_.length).map { case (gen, count) =>
      FishGeneration(gen, count)
    }.toList

    val after256days = (0 until 256).foldLeft(generations) { case (gens, _) =>
      gens.flatMap(_.next()).groupBy(_.gen).values.map { sameGen =>
        sameGen.reduce[FishGeneration] { case (a, b) => a.mergeGenerations(b) }
      }.toList
    }

    println(after256days.map(_.count).sum)

  }


  case class Fish(state: Int) {
    def next(): Seq[Fish] = {
      if (state == 0)
        Fish(6) :: Fish(8) :: Nil
      else
        Fish(state - 1) :: Nil
    }
  }

  object Fish {
    def parse(s: String): Fish = {
      val i  = s.toInt
      assert(i > 0)
      Fish(i)
    }
  }

  // for part 2 need to be smart about saving space and represent a fish generation instead of individuals
  // `count` must be Long - big boi numbers
  case class FishGeneration(gen: Int, count: Long) {

    def mergeGenerations(other: FishGeneration): FishGeneration = {
      assert(other.gen == this.gen)
      FishGeneration(gen, other.count + this.count)
    }

    def next() = {
      if (this.spawnGen)
        FishGeneration(6, count) :: FishGeneration(8, count) :: Nil
      else
        FishGeneration(gen - 1, count) :: Nil
    }

    def spawnGen = gen == 0
  }
}
