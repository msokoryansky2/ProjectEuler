package com.msokoryansky.EulerProblems

import com.msokoryansky.EulerProblems.NumberGrid.{Direction, SortingHat}

/*
In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
  */

class P0011 extends EulerProblem {
  def run: String = NumberGrid(
    """
      |08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
      |49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
      |81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
      |52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
      |22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
      |24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
      |32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
      |67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
      |24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
      |21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
      |78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
      |16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
      |86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
      |19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
      |04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
      |88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
      |04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
      |20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
      |20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
      |01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
    """.stripMargin, SortingHat[Long]((a: Long, b: Long) => a * b, 1
                                  , (x: Long, y: Long) => Math.max(x, y), 0)).
          bestGridResultInAllDirections(4).toString
}

object P0011 extends App {
  (new P0011).printAnswer()
}

class NumberGrid private (longGrid: Array[Array[Long]], sortingHat: NumberGrid.SortingHat[Long]) {
  NumberGrid.validate(longGrid)
  private val grid = longGrid
  private val hat = sortingHat

  override def toString: String = NumberGrid.longGrid2stringGrid(grid)

  def lengthY: Int = grid.length
  def lengthX: Int = {
    require(lengthY > 0, "The grid is empty")
    grid(0).length
  }
  def value(x: Int, y: Int): Long = {
    require(x >= 0 && y >= 0, "Negative coordinates are not allowed")
    require(x < lengthX, "The X coordinate exceeds grid width")
    require(x < lengthX, "The Y coordinate exceeds grid height")
    grid(y)(x)
  }

  def nextValueInDirection(startX: Int, startY: Int, dir: Direction.Value): Option[Long] = {
    val x = startX + Direction.dX(dir)
    val y = startY + Direction.dY(dir)
    if (x >= 0 && y >= 0 && y < lengthY && x < lengthX) Some(value(x, y)) else None
  }

  def nextValuesInDirection(startX: Int, startY: Int, dir: Direction.Value, length: Int): List[Long] = {
    require(length > 0, "Must specify positive number of cells")
    def nextValuesInDirectionAcc(startX: Int, startY: Int, length: Int, acc: List[Long]): List[Long] = {
      if (length <= 0) acc
      nextValueInDirection(startX, startY, dir) match {
        case Some(value) => nextValuesInDirectionAcc(startX + Direction.dX(dir), startY + Direction.dY(dir),
          length - 1, value :: acc)
        case _ => acc
      }
    }
    nextValuesInDirectionAcc(startX, startY, length - 1, List(value(startX, startY))).reverse
  }

  def bestCellResultInAllDirections(startX: Int, startY: Int, length: Int): Long = {
    Direction.values.map{(dir) => nextValuesInDirection(startX, startY, dir, length).foldLeft(hat.calcAcc)(hat.calc)}
                                                    .foldLeft(hat.selectAcc)(hat.select)
  }

  def bestGridResultInAllDirections(length: Int): Long = {
    def bestGridResultInAllDirectionsAcc(startX: Int, startY: Int, acc: Long): Long = {
      val best = (bestCellResultInAllDirections(startX, startY, length) :: acc :: Nil).foldLeft(hat.calcAcc)(hat.calc)
      if (startY >= grid.length - 1 && startX >= grid(startY).length - 1) best
      else {
        val nextX = if (startX <= grid(startY).length - 1) startX + 1 else 0
        val nextY = if (nextX == 0) startY + 1 else startY
        bestGridResultInAllDirectionsAcc(nextX, nextY, best)
      }
    }
    bestGridResultInAllDirectionsAcc(0, 0, hat.calcAcc)
  }
}

object NumberGrid {
  private def validate(grid: Array[Array[Long]]) = {
    require(grid.length > 0, "The number grid is empty")
    require(grid(0).length > 0, "First row in the number grid is empty")
    require(grid.forall(_.length == grid(0).length), "Not all rows in the number grid have the same number of elements")
  }

  def stringGrid2longGrid(stringGrid: String):  Array[Array[Long]] = {
    val rows: Array[String] = stringGrid.split("\\r\\n|\\n|\\r").filterNot{(s) => s.trim.isEmpty}
    rows.map{(row) => row.split("\\s+").map{(s) => s.toLong}}
  }

  def longGrid2stringGrid(longGrid: Array[Array[Long]]): String = {
    val maxDigits = longGrid.flatten.map{_.toString.length}.max
    longGrid.map{ (row) => row.map{(n) => ("%0" + maxDigits + "d").format(n)}.mkString(" ")}.mkString("\n")
  }

  case class SortingHat[A](calc: (A, A) => A, calcAcc: A, select: (A, A) => A, selectAcc: A)

  def apply(longGrid: Array[Array[Long]], hat: SortingHat[Long]) = new NumberGrid(longGrid, hat)
  def apply(stringGrid: String, hat: SortingHat[Long]) =  new NumberGrid(stringGrid2longGrid(stringGrid), hat)

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
  }
}