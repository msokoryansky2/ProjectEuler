package com.msokoryansky.MathUtils

/**
  * Array-of-Array representation of a pyramid. 0th element of outer array should have array of 1 element,
  * 1st outer element => array of 2 elements, Nth element of outer array => inner array of N+1 elements
  * @param longPyramid
  */
class NumberPyramid private (longPyramid: Array[Array[Long]]) {
  NumberPyramid.validate(longPyramid)
  val pyramid = longPyramid

  def bestPath(row: Int, element: Int, hat: SortingHat[Long]): Unit = {

  }

  override def toString(): String = NumberPyramid.toString(pyramid)
}

object NumberPyramid extends App {

  println(NumberPyramid(
    """
      |75
      |95 64
      |17 47 82
      |18 35 87 10
      |20 04 82 47 65
      |19 01 23 75 03 34
      |88 02 77 73 07 63 67
      |99 65 04 28 06 16 70 92
      |41 41 26 56 83 40 80 70 33
      |41 48 72 33 47 32 37 16 94 29
      |53 71 44 65 25 43 91 52 97 51 14
      |70 11 33 28 77 73 17 78 39 68 17 57
      |91 71 52 38 17 14 91 43 58 50 27 29 48
      |63 66 04 68 89 53 67 30 73 16 69 87 40 31
      |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
    """.stripMargin))


  def validate(longPyramid: Array[Array[Long]]): Unit = {
    require(longPyramid.length > 0, "Empty pyramid")
    for {
      n <- 0 to longPyramid.length - 1
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

/**
  * Judge of bestness of list of lists of values
  * @param calc folder of inner list. E.g. add all values
  * @param calcAcc accumulator for for folder of inner list
  * @param select folder of outer list. E.g. find max of values
  * @param selectAcc accumulator for folder of outer list
  * @tparam A e.g. Long
  */
case class SortingHat[A](calc: (A, A) => A, calcAcc: A, select: (A, A) => A, selectAcc: A)