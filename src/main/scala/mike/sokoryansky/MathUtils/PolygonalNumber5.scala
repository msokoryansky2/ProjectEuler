package mike.sokoryansky.MathUtils

/**
  * Legacy wrapper around PolygonalObject
  */
object PolygonalNumber5 {
  def pentagonalNumber(n: Long): Long = PolygonalNumber(5).number(n)

  def getPentagonalNumberIndex(pn: Long): Option[Long] = PolygonalNumber(5).indexOf(pn)

  def isPentagonalNumber(pn: Long): Boolean = PolygonalNumber(5).isNumber(pn)

  def pentagonalNumbers(n: Long = 1): Stream[Long] = PolygonalNumber(5).numbers(n)

  def distanceBetweenPentagonalNumbers(n: Long, m: Long): Long = PolygonalNumber(5).distance(n, m)

  def precedingPentagonalNumber(target: Long): Long = PolygonalNumber(5).indexOfFractional(target).floor.toInt

  def succeedingPentagonalNumber(target: Long): Long = PolygonalNumber(5).indexOfFractional(target).ceil.toInt

  def firstRemotePentagonalNumber(distance: Long): Long = PolygonalNumber(5).indexOfFirstRemote(distance).toInt

  def firstPentagonalPairWithProperty(start: Long, p: (Long, Long) => Boolean): (Long, Long) =
    PolygonalNumber(5).indexOfFirstPairWithProperty(start, p)

  def allPentagonalPairsWithinDistanceWithProperty(start: Long, finish: Long, distance: Long,
                                                    p: (Long, Long) => Boolean): List[(Long, Long)] =
    PolygonalNumber(5).indexOfAllPairsWithinDistanceWithProperty(start, finish, distance, p)

  def propertyPentagonalSumAndDiff(n1: Long, n2: Long): Boolean = PolygonalNumber(5).polygonalSumAndDiff(n1, n2)
}
