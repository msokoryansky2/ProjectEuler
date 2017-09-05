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
case class NGon(numbers: List[Int]) extends Ordered[NGon] {
  require(numbers.size % 2 == 0, "Must have even number of numbers for vertices and spokes")
  require(numbers.size >= 6, "Must have at least 6 numbers specified for a 3-gon")
  val sides: Int = numbers.size / 2
  lazy val (outer, inner) = numbers.toIndexedSeq.splitAt(sides)
  lazy val s: Int = outer.zipWithIndex.min._2
  lazy val lines: Seq[Seq[Int]] = 
    (0 until sides).map(i => List(outer((i + s) % sides), inner((i + s) % sides), inner((i + s + 1) % sides)))
  lazy val lineValues: Seq[String] = lines.map(_.mkString)
  lazy val value: String = lineValues.mkString

  def compare(other: NGon): Int = BigInt(this.value) compare BigInt(other.value)
  def canEqual(other: NGon): Boolean = other.isInstanceOf[NGon] && other.sides == this.sides
  override def equals(that: Any): Boolean =
    that.isInstanceOf[NGon] && that.asInstanceOf[NGon].canEqual(this) && this.compare(that.asInstanceOf[NGon]) == 0
}

object NGon {
  def apply(numbers: List[Int]) = new NGon(numbers)
  /**
    * Produce all possible but different ngons. Two ngons are different from one another if they can't be roteted
    * to be identical. Note that the outer spokes rotate together with the inner polygon.
    */
  def ngons(numbers: Set[Int]): Seq[NGon] = Permutation.permutations2(numbers).map(NGon(_))

  /**
    * Produce all permutations of NGons with pre-specified inner and outer vertices
    */
  def ngons(outer: Set[Int], inner: Set[Int]): Seq[NGon] = {
    val outers = Permutation.permutations2(outer)
    val inners = Permutation.permutations2(inner)
    outers.flatMap(o => inners.map(i => o ++ i)).map(NGon(_))
  }
}