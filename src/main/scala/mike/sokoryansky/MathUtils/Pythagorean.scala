package mike.sokoryansky.MathUtils

object Pythagorean {
  /**
    * Returns all right triangles (each tri as a sides-triple) with integer sides (aka Pythagorean Triples)
    * for the specified perimeter.
    *
    * There is an optional predicate parameter to to filter down to only those right triangles that meet the predicate.
    *
    * We know that if sides are a, b, and c and perimeter is L, then:
    * a**2 + b**2 = c**2  and a + b + c + L
    * a**2 + b**2 = (L - a - b)**2
    * ...
    * b = (L*L - 2*L*a)/(2*L - 2*a)
    *
    * If we assume that a is the shorter of two non-hypotenuse sides, we know it's no longer than L/4 because
    * no side is longer than L/2 and we assumed that a is the shorter side.
    *
    * We also know that smallest triple has perim of 12 and perim of 3.
    *
    * We know that perimeter of a pythagorean triple is always even. Proof:
    * If a is even and b is even, then a and b squared are even, then c squared is even, then c is even
    *     => even perim (sum of even + even + even)
    * If a is odd and b is even, a odd is even and b squared is even, then c squared is odd, then c is odd
    *     => even perim (sum of odd + odd + even)
    * If is odd and b is odd, then a nd b squared are odd, then c squared is even, then c is even
    *     => even perim (sum of odd + odd + even)
    */
  def pythagoreanTriples(per: Long,
                         pA: (Long) => Boolean = (_) => true,
                         pABC: ((Long, Long, Long)) => Boolean = (_) => true): List[(Long, Long, Long)] =
    if (per < 12 || per % 2 == 1) List[(Long, Long, Long)]()
    else (for {
      a <- 3L to per / 4
      if pA(a)
      b = pythagoreanB(a, per).getOrElse(0L)
      if b > 0
      c = per - a - b
      if pABC(a, b, c)
    } yield (a, b, c)).toList

  /**
    * All pythagorean triples for all numbers from 1 to N that are NOT trivial multiples of smaller pythagorean triples.
    * I.e. once we generate (3,4,5) we won't generate (6,8,10), (9,12,15), etc.
    *
    * As explained in pythagoreanTriples comment, we know that all perimeters of pythagorean triangles are even, so we
    * start with 12 (smallest such triangle) and increment by 2
    */
  def pythagoreanTriplesNonTrivial1ToN(n: Long): Map[Long, List[(Long, Long, Long)]] = {
    def pythagoreanTriplesNonTrivial1ToNAcc(per: Long,
                                  acc: Map[Long, List[(Long, Long, Long)]]): Map[Long, List[(Long, Long, Long)]] = {
      if (per > n) acc
      else {
        val mults: Map[Long, List[(Long, Long, Long)]] = acc.filter(pt => per % pt._1 == 0)
        val multsA: List[Long] = mults.flatMap(_._2).map(_._1).toList
        val newTriples = pythagoreanTriples(per, a => !multsA.exists(ma => a % ma == 0))
        pythagoreanTriplesNonTrivial1ToNAcc(per + 2, if (newTriples.nonEmpty) acc ++ Map(per -> newTriples) else acc)
      }
    }
    pythagoreanTriplesNonTrivial1ToNAcc(12, Map())
  }

  /**
    * Returns pythagorean triple B option for the given A and sum of A, B, C (i.e. perimeter) where C is hypotenuse.
    */
  protected def pythagoreanB(a: Long, per: Long): Option[Long] = {
    require(per > 2 * a, "Perimeter must be more than twice the length of any one side")
    val bNum = per * per - 2 * per * a
    val bDenom = 2 * per - 2 * a
    if (bNum % bDenom == 0) Some(bNum / bDenom) else None
  }
}
