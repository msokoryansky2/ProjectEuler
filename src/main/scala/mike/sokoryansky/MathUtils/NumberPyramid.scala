package mike.sokoryansky.MathUtils

/**
  * Array-of-Array representation of a pyramid. 0th element of outer array should have array of 1 element,
  * 1st outer element => array of 2 elements, Nth element of outer array => inner array of N+1 elements
  * @param longPyramid array-of-array of longs representing the pyramid
  */
class NumberPyramid private (longPyramid: Array[Array[Long]]) {
  NumberPyramid.validate(longPyramid)
  val pyramid: Array[Array[Long]] = longPyramid
  val height: Int  = pyramid.length
  /**
    * Returns 1-indexed array in place y of row x
    * @param x 1-indexed place number in row (e.g: 5, 6, 7, 8  has 7 in 3rd place)
    * @param y 1-indexed row number
    * @return option of long number that's element of the pyramid, provided x and y are valid
    */
  def element(x: Int, y: Int): Long = {
    require(isElement(x, y), s"($x, $y) is not a valid element position in this pyramid")
    pyramid(y - 1)(x - 1)
  }
  def isElement(x: Int, y: Int): Boolean = x >= 1 && y >= 1 && y <= height && x <= pyramid(y - 1).length

  def bestPathFromPeak(hat: SortingHat[Long]): List[(Int, Int)] = allPaths(hat)(1, 1)

  def allPaths(hat: SortingHat[Long]): Map[(Int, Int), List[(Int, Int)]] = {
    def allPathsAcc(x: Int, y: Int, acc: Map[(Int, Int), List[(Int, Int)]]): Map[(Int, Int), List[(Int, Int)]] = {
      (x, y) match {
        case (x1, y1) if !isElement(x1, y1) && (y1 > 1 && y1 <= height) => allPathsAcc(1, y - 1, acc)
        case (x1, y1) if !isElement(x1, y1) => acc        // covers both reaching pyramid peak and invalid (x, y)
        case (x1, y1) =>
          val left: List[(Int, Int)] = (x1, y1) :: (if (acc.contains(x, y + 1)) acc(x, y + 1) else Nil)
          val right: List[(Int, Int)] = (x1, y1) :: (if (acc.contains(x + 1, y + 1)) acc(x + 1, y + 1) else Nil)
          (left.map(el => element(el._1, el._2)).foldLeft(hat.calcAcc)(hat.calc),
            right.map(el => element(el._1, el._2)).foldLeft(hat.calcAcc)(hat.calc)) match {
            case (l, r) if List(l, r).foldLeft(hat.selectAcc)(hat.select) == l =>
              allPathsAcc(x + 1, y, Map((x, y) -> left) ++ acc)
            case (l, r) =>
              allPathsAcc(x + 1, y, Map((x, y) -> right) ++ acc)
          }
      }
    }
    allPathsAcc(1, pyramid.length, Map[(Int, Int), List[(Int, Int)]]())
  }

  override def toString: String = NumberPyramid.toString(pyramid)
}

object NumberPyramid extends App {
  def validate(longPyramid: Array[Array[Long]]): Unit = {
    require(longPyramid.length > 0, "Empty pyramid")
    for {
      n <- longPyramid.indices
    } yield require(longPyramid(n).length == n + 1, "Row " + (n + 1) + " does not have expected length of " + (n + 2))
  }

  def fromString(str: String): Array[Array[Long]] = {
    val rows: Array[String] = str.split("\\r\\n|\\n|\\r").filterNot {s => s.trim.isEmpty}
    rows.map(_.split("\\s+").map(_.toLong))
  }

  def toString(pyramid: Array[Array[Long]]): String = {
    /**
      * Centers a string in a line of length characters, padded on both sides by spaces
      * @param str string to be centered
      * @param length length of resulting string of *spaces*str*spaces
      * @return
      */
    def center(str: String, length: Int): String = {
      " " * ((length - str.length) / 2) + str + " " * (length - ((length - str.length) / 2))
    }
    val numDigits = pyramid.flatten.map(_.toString.length).max
    val numCharsTotal = pyramid(pyramid.length - 1).length * (numDigits + 1) - 1
    pyramid
      .map(row => center(row.map(("%0" + numDigits.toString + "d").format(_)).mkString(" "), numCharsTotal))
      .mkString(sys.props("line.separator"))
  }

  def apply(str: String) = new NumberPyramid(fromString(str))
  def apply(longPyramid: Array[Array[Long]]) = new NumberPyramid(longPyramid)
}
