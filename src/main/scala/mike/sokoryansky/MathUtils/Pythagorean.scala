package mike.sokoryansky.MathUtils

object Pythagorean {
  /**
    * My algo. Sadly, slow.
    *
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
    * If we assume that a is the shorter of two non-hypotenuse sides, we know it's no longer than L/3 because
    * we know it's the shortest of 3 sides and if it's >= L/3 then hypotenuse wouldn't be longer than two other sides.
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
                         pABC: ((Long, Long, Long)) => Boolean = (_) => true): List[(Long, Long, Long)] = {
    /**
      * Returns pythagorean triple B option for the given A and sum of A, B, C (i.e. perimeter) where C is hypotenuse.
      */
    def pythagoreanB(a: Long, per: Long): Option[Long] = {
      require(per > 2 * a, "Perimeter must be more than twice the length of any one side")
      val bNum = per * per - 2 * per * a
      val bDenom = 2 * per - 2 * a
      if (bNum % bDenom == 0) Some(bNum / bDenom) else None
    }

    if (per < 12 || per % 2 == 1) List[(Long, Long, Long)]()
    else (for {
      a <- 3L to per / 3
      if pA(a)
      b = pythagoreanB(a, per).getOrElse(0L)
      if b > a
      c = per - a - b
      if pABC(a, b, c)
    } yield (a, b, c)).toList
  }

  /**
    * My algo. Sadly, slow.
    *
    * All pythagorean triples for all numbers from 1 to N that are NOT trivial multiples of smaller pythagorean triples.
    * I.e. once we generate (3,4,5) we won't generate (6,8,10), (9,12,15), etc.
    *
    * As explained in pythagoreanTriples comment, we know that all perimeters of pythagorean triangles are even, so we
    * start with 12 (smallest such triangle) and increment by 2
    */
  def pythagoreanTriplesPrimitive1ToN(n: Long): Map[Long, List[(Long, Long, Long)]] = {
    def pythagoreanTriplesPrimitive1ToNAcc(per: Long,
                                  acc: Map[Long, List[(Long, Long, Long)]]): Map[Long, List[(Long, Long, Long)]] = {
      if (per > n) acc
      else {
        val mults: Map[Long, List[(Long, Long, Long)]] = acc.filter(pt => per % pt._1 == 0)
        val newTriples = pythagoreanTriples(per,
          a => !mults.exists(mult => per % mult._1 == 0 &&
                              mult._2.exists(t => a % t._1 == 0 && a / t._1 ==  per / mult._1)))
        pythagoreanTriplesPrimitive1ToNAcc(per + 2, if (newTriples.nonEmpty) acc ++ Map(per -> newTriples) else acc)
      }
    }
    pythagoreanTriplesPrimitive1ToNAcc(12, Map())
  }


  /**
    * Not my algo. But fast :)
    *
    * Fast Pythagorean Triple generator for both primitive and trivial (multiples of primitives) triples
    * When m and n are any two positive integers n, m such that n > m:
    *
    * a = n**2 - m**2
    * b = 2nm
    * c = n**2 + m**2
    *
    * Then a, b, and c form a primitive Pythagorean Triple. See https://en.wikipedia.org/wiki/Pythagorean_triple
    *
    * Note: to generate only primitive triples, m and n need to be co-prime and of different parity (one even, one odd).
    *
    * Because a + b + c = 2n**2 + 2nm = perimeter, for any given perimeter, maximum n is sqrt(perim/2)
    */
  def pythagoreanTriples1ToN(perMax: Long, primitiveOnly: Boolean = false): Map[Long, List[(Long, Long, Long)]] = {
    val nMax = Math.sqrt(perMax / 2).toLong
    // We only need map of co-primes for primitive triples calc. For that scenario we also need to filter out
    // co-primes of n that have the same parity (evenness/oddness) as n, which effectively means that either n or its
    // co-primes have to be even.
    val coprimesThruNMax: Map[Long, Seq[Long]] =
      if (primitiveOnly) Prime.relativePrimesThroughN(nMax).map(np =>
        np._1 -> np._2.filter(p => np._1 % 2 == 0 || p% 2 == 0))
      else Map[Long, Seq[Long]]()
    // Map of possible N's to possible M's for each N
    val nm: Map[Long, Seq[Long]] =
      (2L to nMax).map(n => n -> (if (primitiveOnly) coprimesThruNMax(n) else (1L until n).toList)).toMap
    val triples: List[(Long, (Long, Long, Long))] = (for {
      n <- nm.keys
      m <- nm(n)
      nn = n * n
      mm = m * m
      a = nn - mm
      b = 2 * n * m
      c = nn + mm
      per = a + b + c
      if per <= perMax
    } yield (per, (Math.min(a, b), Math.max(a, b), c))).toList
    triples.groupBy(_._1).map(t => t._1 -> t._2.map(_._2))
  }
}
