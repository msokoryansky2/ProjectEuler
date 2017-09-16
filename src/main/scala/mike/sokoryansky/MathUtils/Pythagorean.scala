package mike.sokoryansky.MathUtils

object Pythagorean {
  /**
    * Returns all right traingles (each tri as a sides-triple) with integer sides for the specified perimeter.
    * There is an optional predicate parameter to to filter down to only those right triangles that meet the predicate.
    *
    * We know that if sides are a, b, and c and perimeter is L, then:
    * a**2 + b**2 = c**2  and a + b + c + L
    * a**2 + b**2 = (L - a - b)**2
    * ...
    * b = (L*L - 2*L*a)/(2*L - 2*a)
    *
    * If we assume that a is the shorter of two non-hypotenuse sides, we know it's no longer than L/4 because
    * no side is longer than L/2 and we assumed that a is the shorter side
    */
  def pythagoreanTriple(per: Long, p: ((Long, Long, Long)) => Boolean = (_) => true): List[(Long, Long, Long)] =
    if (per < 12) List[(Long, Long, Long)]()
    else (for {
      a <- 1L to per / 4
      b = pythagoreanB(a, per).getOrElse(0L)
      if b > 0
      c = per - a - b
      if p(a, b, c)
    } yield (a, b, c)).toList

  /**
    * Returns pythagorean triple B option for the given A and sum of A, B, C (i.e. perimeter) where C is hypotenuse
    */
  def pythagoreanB(a: Long, per: Long): Option[Long] = {
    val bNum = per * per - 2 * per * a
    val bDenom = 2 * per - 2 * a
    if (bNum > 0 && bDenom > 0 && bNum % bDenom == 0) Some(bNum / bDenom) else None
  }
}
