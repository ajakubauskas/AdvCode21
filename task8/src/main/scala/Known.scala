
trait Signal

trait Known extends Signal
case class MustBe(s: Set[Char], num: Int) extends Known {

}

object MustBe {
  def parse(s: String): Option[MustBe] = for {
    num <- s.length match {
      case 2 => Some (1)
      case 4 => Some (4)
      case 3 => Some (7)
      case 7 => Some (8)
      case _ => None
    }
  } yield MustBe(s.toSet, num)
}

trait Position extends Known
case class HTop(c: Char) extends Position
case class HMiddle(c: Char) extends Position
case class HBottom(c: Char) extends Position
case class VRightTop(c: Char) extends Position
case class VRightBottom(c: Char) extends Position
case class VLeftTop(c: Char) extends Position
case class VLeftBottom(c: Char) extends Position


trait MayBe extends Signal
case class MayBe235(s: Set[Char]) extends MayBe
case class MayBe25(s: Set[Char]) extends MayBe
case class MayBe69(s: Set[Char]) extends MayBe

object MayBe {
  def parse(s: String): Option[MayBe] =
    s.length match {
      case 5 => Some(MayBe235(s.toSet))
      case 6 => Some(MayBe69(s.toSet))
      case _ => None
    }
}

object Signal {
  def parse(s: String) = MustBe.parse(s) orElse MayBe.parse(s)
}