object DeriveNumber {



  // we can only derive new knowledge if we combine something we don't know yet with something we know
  def derive: (MayBe, Known) => Seq[Signal] = {
//    // dummy rule, establish general pattern
//    case (a: Known, b: MayBe) =>
//      b:: Nil

  /*
  some possible observations we can make:

  * 2 overlap with 4 in 2 chars +
  * 5 overlap with 4 in 2 chars +
  * 3 overlaps with 4 in 3 chars +
  * 2 overlap with 7 in 2 chars +
  * 5 overlap with 7 in 2 chars +
  * 2 overlap with 1 in 1 chars +
  * 5 overlap with 1 in 1 chars +
  * 3 overlaps with 1 in 2 chars +
  * 6 overlaps with 1 in 1 chars +
  * 9 overlaps with 1 in 2 chars +
  * 6 overlaps with 7 in 2 chars +
  * 9 overlaps with 7 in 3 chars +
  * 6 overlaps with 4 in 3 chars +
  * 9 overlaps with 4 in 4 chars +

   */
    // both Num 2 and Num 5 share exactly 2 letter with Num 4
    case (a: MayBe235, MustBe(s, 4)) if a.s.intersect(s).size == 2 =>
      MayBe25(a.s) :: Nil

    // both Num 2 and Num 5 share exactly 2 letter with Num 4
    case (a: MayBe235, MustBe(s, 4)) if a.s.intersect(s).size == 3 =>
      MustBe(a.s, 3) :: Nil

    // Num 3 shares exactly 2 letters with a Num 1
    case (a: MayBe235, MustBe(s, 1)) if a.s.intersect(s).size == 2 =>
        MustBe(a.s, 3):: Nil

    // both Num 2 and Num 5 share exactly 1 letter with Num 1
    case (a: MayBe235, MustBe(s, 1)) if a.s.intersect(s).size == 1 =>
      MayBe25(a.s) :: Nil

    // Num 3 shares exactly 3 letters with a Num 4
    case (a: MayBe235, MustBe(s, 4)) if a.s.intersect(s).size == 3 =>
        MustBe(a.s, 3):: Nil

    // both Num 2 and Num 5 share exactly 2 letter with Num 4
    case (a: MayBe235, MustBe(s, 4)) if a.s.intersect(s).size == 2 =>
      MayBe25(a.s) :: Nil

    // Num 3 shares exactly 3 letters with a Num 7
    case (a: MayBe235, MustBe(s, 7)) if a.s.intersect(s).size == 3 =>
        MustBe(a.s, 3):: Nil

    // both Num 2 and Num 5 share exactly 2 letter with Num 7
    case (a: MayBe235, MustBe(s, 7)) if a.s.intersect(s).size == 2 =>
      MayBe25(a.s) :: Nil

    // Num 6 shares exactly 1 letter with a Num 1
    case (a: MayBe69, MustBe(s, 1)) if a.s.intersect(s).size == 1 =>
        MustBe(a.s, 6):: Nil

    // Num 9 shares exactly 2 letter with Num 1
    case (a: MayBe69, MustBe(s, 1)) if a.s.intersect(s).size == 2 =>
      MustBe(a.s, 9) :: Nil

    // Num 6 shares exactly 2 letter with a Num 7
    case (a: MayBe69, MustBe(s, 7)) if a.s.intersect(s).size == 2 =>
      MustBe(a.s, 6):: Nil

    // Num 9 shares exactly 3 letter with Num 7
    case (a: MayBe69, MustBe(s, 7)) if a.s.intersect(s).size == 3 =>
      MustBe(a.s, 9) :: Nil

    // Num 6 shares exactly 3 letter with a Num 4
    case (a: MayBe69, MustBe(s, 4)) if a.s.intersect(s).size == 3 =>
        MustBe(a.s, 6):: Nil

    // Num 9 shares exactly 4 letter with Num 4
    case (a: MayBe69, MustBe(s, 4)) if a.s.intersect(s).size == 4 =>
      MustBe(a.s, 9) :: Nil

    case _ => Nil

  }

  def deriveFromMayBe: (MayBe, Iterable[Known]) => Seq[Signal] = { case (maybe, knowns) =>
    knowns.flatMap(derive(maybe, _)).toSeq
  }

//  def derive: (Known, MayBe) => Seq[Number] = { (known, maybe) =>
//    this.derive(maybe, known)
//  }

}
