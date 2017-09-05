package mike.sokoryansky.MathUtils

/**
  * N-gon is a polygon with spokes as described in https://projecteuler.net/problem=68
  * It is defined by numbers that go into its vertices. Number of sides = numbers.size / 2.
  * First half of numbers go into outside spokes vertices, clockwise. Second half of numbers go into
  * polygon vertices, the first one starting "under" the first outside one.
  *
  * We don't care which outside spoke is first, since it's just a matter of turning the polygon. What matters is
  * that we follow clockwise from the first.
  */
case class NGon(numbers: Set[Long]) {
  require(numbers.size % 2 == 0, "Must have even number of numbers for vertices and spokes")
  require(numbers.size >= 6, "Must have at least 6 numbers specified for a 3-gon")
  val sides: Int = numbers.size / 2
  val (outer, inner) = numbers.toIndexedSeq.splitAt(sides)
  lazy val lines: Seq[Seq[Long]] = (0 until sides).map(i => List(outer(i), inner(i), inner((i + 1) % sides)))
  lazy val lineValues: Seq[String] = lines.map(_.mkString)
  lazy val ngonValue: String = lineValues.mkString
}

object NGon {
  def apply(numbers: Set[Long]) = new NGon(numbers)
  /**
    * Produce all possible but different ngons. Two ngons are different from one another if they can't be roteted
    * to be identical. Note that the outer spokes rotate together with the inner polygon.
    */
  def ngons(numbers: Set[Long]): Seq[NGon] = {
    Permutation.permutations2(numbers).map(l => NGon(l.toSet))
  }
}