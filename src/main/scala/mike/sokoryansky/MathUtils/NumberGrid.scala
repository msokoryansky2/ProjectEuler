package mike.sokoryansky.MathUtils

import mike.sokoryansky.MathUtils.NumberGrid.Direction

class NumberGrid private (longGrid: Array[Array[Long]]) {
  NumberGrid.validate(longGrid)
  private val grid = longGrid

  override def toString: String = NumberGrid.longGrid2stringGrid(grid)

  /**
    *
    * @return number of rows in the grid
    */
  def lengthY: Int = grid.length

  /**
    *
    * @return number of elements in a row (under requirement of non-empty and assumption of rectangular grid)
    */
  def lengthX: Int = {
    require(lengthY > 0, "The grid is empty")
    grid(0).length
  }

  /**
    *
    * @param x 0-indexed number of element in row
    * @param y 0-indexed number of row in grid
    * @return value in 0-indexed X row / Y column of the grid
    */
  def value(x: Int, y: Int): Long = {
    require(x >= 0 && y >= 0, "Negative coordinates are not allowed")
    require(x < lengthX, "The X coordinate exceeds grid width")
    require(y < lengthY, "The Y coordinate exceeds grid height")
    grid(y)(x)
  }

  /**
    * Neighboring cell value in specified direction from starting cell, with None if neighbor doesn't exist
    * @param startX 0-indexed number of starting element in row
    * @param startY 0-indexed number of row in grid
    * @param dir one of defined Directions (N, NE, E, SE, S, SW, W, NW)
    * @return option of neighbor value
    */
  def nextValueInDirection(startX: Int, startY: Int, dir: Direction.Value): Option[Long] = {
    val x = startX + Direction.dX(dir)
    val y = startY + Direction.dY(dir)
    if (x >= 0 && y >= 0 && y < lengthY && x < lengthX) Some(value(x, y)) else None
  }

  /**
    * List of cell values beginning with specified start cell and of specified length in specified direction
    * @param startX 0-indexed number of starting element in row
    * @param startY 0-indexed number of row in grid
    * @param dir one of defined Directions (N, NE, E, SE, S, SW, W, NW)
    * @param length how many cells to take in specified direction. Length of 1 results in starting cell only
    * @return list of values in specified direction
    */
  def nextValuesInDirection(startX: Int, startY: Int, dir: Direction.Value, length: Int): List[Long] = {
    if (length == 0) List[Long]()
    else if (length < 0) nextValuesInDirection(startX, startY, NumberGrid.Direction.reverse(dir), 0 - length)
    else {
      def nextValuesInDirectionAcc(startX: Int, startY: Int, length: Int, acc: List[Long]): List[Long] = {
        if (length <= 0) acc
        else nextValueInDirection(startX, startY, dir) match {
          case Some(value) => nextValuesInDirectionAcc(startX + Direction.dX(dir), startY + Direction.dY(dir),
            length - 1, value :: acc)
          case _ => acc
        }
      }
      nextValuesInDirectionAcc(startX, startY, length - 1, List(value(startX, startY))).reverse
    }
  }

  /**
    * Applies SortingHat to decide which of defined directions provides "best" result of specified length from
    * specified start. SortingHat uses calc to fold list of values in each direction to a single value and then uses
    * select to decide which of directions is the "best"
    * @param startX 0-indexed number of starting element in row
    * @param startY 0-indexed number of row in grid
    * @param length how many cells to take in specified direction. Length of 1 results in starting cell only
    * @return "best" available result from starting cell of specified length and given current SortingHat
    */
  def bestCellResultInAllDirections(startX: Int, startY: Int, length: Int, hat: SortingHat[Long]): Long = {
    Direction.values.map{(dir) => nextValuesInDirection(startX, startY, dir, length).foldLeft(hat.calcAcc)(hat.calc)}
      .foldLeft(hat.selectAcc)(hat.select)
  }

  /**
    * Applies SortingHat to every cell in the grid to find "best" possible result of length cells
    * @param length how many cells to look at in each direction. Length of 1 means one cell
    * @param hat SortingHat to judge bestness
    * @return "best" result available in the grid
    */
  def bestGridResultInAllDirections(length: Int, hat: SortingHat[Long]): Long = {
    def bestGridResultInAllDirectionsAcc(startX: Int, startY: Int, acc: Long): Long = {
      val best = (bestCellResultInAllDirections(startX, startY, length, hat) :: acc :: Nil)
        .foldLeft(hat.selectAcc)(hat.select)
      if (startY >= lengthY - 1 && startX >= lengthX - 1) best
      else {
        val nextX = if (startX < lengthX - 1) startX + 1 else 0
        val nextY = if (nextX == 0) startY + 1 else startY
        bestGridResultInAllDirectionsAcc(nextX, nextY, best)
      }
    }
    bestGridResultInAllDirectionsAcc(0, 0, hat.selectAcc)
  }
}

/**
  * Judge of bestness of list of lists of values
  * @param calc folder of inner list. E.g. multiply all values
  * @param calcAcc accumulator for for folder of inner list
  * @param select folder of outer list. E.g. find max of values
  * @param selectAcc accumulator for folder of outer list
  * @tparam A e.g. Long
  */
case class SortingHat[A](calc: (A, A) => A, calcAcc: A, select: (A, A) => A, selectAcc: A)

object NumberGrid {
  /**
    * Validate parsed grid. Expect non-emptiness and rectangularity
    *
    * @param grid parsed grid in array-of-array form
    */
  private def validate(grid: Array[Array[Long]]) = {
    require(grid.length > 0, "The number grid is empty")
    require(grid(0).length > 0, "First row in the number grid is empty")
    require(grid.forall(_.length == grid(0).length), "Not all rows in the number grid have the same number of elements")
  }

  /**
    *
    * @param stringGrid string representation of the grid with newlines separating rows
    *                   and whitespace separating elements within each row
    * @return parsed array-of-arrays representation of the grid
    */
  def stringGrid2longGrid(stringGrid: String): Array[Array[Long]] = {
    val rows: Array[String] = stringGrid.split("\\r\\n|\\n|\\r").filterNot { (s) => s.trim.isEmpty }
    rows.map { (row) => row.split("\\s+").map { (s) => s.toLong } }
  }

  /**
    *
    * @param longGrid parsed grid in array-of-arrays form
    * @return pretty-printable string representation of parsed grid
    */
  def longGrid2stringGrid(longGrid: Array[Array[Long]]): String = {
    val maxDigits = longGrid.flatten.map {
      _.toString.length
    }.max
    longGrid.map { (row) => row.map { (n) => ("%0" + maxDigits + "d").format(n) }.mkString(" ") }.mkString("\n")
  }

  def apply(longGrid: Array[Array[Long]]) = new NumberGrid(longGrid)

  def apply(stringGrid: String) = new NumberGrid(stringGrid2longGrid(stringGrid))

  object Direction extends Enumeration {
    val N, NE, E, SE, S, SW, W, NW = Value

    def dX(dir: Direction.Value): Int = dir match {
      case N | S => 0
      case NE | E | SE => 1
      case NW | W | SW => -1
    }

    def dY(dir: Direction.Value): Int = dir match {
      case E | W => 0
      case NE | N | NW => -1
      case SE | S | SW => 1
    }

    def reverse(dir: Direction.Value): Direction.Value = dir match {
      case N => S
      case NE => SW
      case E => W
      case SE => NW
      case S => N
      case SW => NE
      case W => E
      case NW => SE
    }
  }
}