package com.msokoryansky.MathUtils

import scala.annotation.tailrec

class NumberSpiral(sideLength: Int, firstValue: Int = 1, direction: Direction.Value = Direction.R) {
  require(sideLength > 0 && sideLength % 2 == 1, "Number spiral side length must be an odd positive number")
  val spiral: Vector[Vector[Long]] = NumberSpiral.fillSpiral(sideLength, firstValue)

  override def toString: String = NumberGrid.longGrid2stringGrid(spiral.map(_.toArray).toArray)

  def row(i: Int): Vector[Long] = {
    require(i >= 0 && i < sideLength, s"Invalid row $i")
    spiral(i)
  }

  def col(i: Int): Vector[Long] = {
    require(i >= 0 && i < sideLength, s"Invalid col $i")
    spiral.map(_(i))
  }

  def cell(x: Int, y: Int): Long = {
    require(x >= 0 && x < sideLength, s"Invalid row $x")
    require(y >= 0 && y < sideLength, s"Invalid col $y")
    spiral(y)(x)
  }

  def diag1: Vector[Long] = spiral.zipWithIndex.map(n => n._1(n._2))

  def diag2: Vector[Long] = spiral.zipWithIndex.map(n => n._1(sideLength - 1 - n._2))
}

object NumberSpiral {
  val init: Long = -999999999

  def initialized(n: Long): Boolean = n != init

  def apply(sideLength: Int, firstValue: Int = 1, direction: Direction.Value = Direction.R): NumberSpiral =
    new NumberSpiral(sideLength, firstValue, direction)

  def fillSpiral(sideLength: Int, firstValue: Int = 1,
                 direction: Direction.Value = Direction.R): Vector[Vector[Long]] = {
    @tailrec def fillSpiralAcc(x: Int, y: Int, value: Long, direction: Direction.Value,
                               acc: Vector[Vector[Long]]): Vector[Vector[Long]] = {
      if (x < 0 || y < 0 || x >= sideLength || y >= sideLength || acc(y)(x) >= 0) acc
      else {
        // a little ugly if this is the first element in the spiral -- in that case we always go in specified direction.
        val nextDir = if (x == (sideLength - 1) / 2 && x == y) direction
                      else nextDirection(sideLength, x, y, direction, acc)
        fillSpiralAcc(nextX(x, nextDir), nextY(y, nextDir), value + 1, nextDir,
          acc.updated(y, acc(y).updated(x, value)))
      }
    }
    fillSpiralAcc((sideLength - 1) / 2, (sideLength - 1) / 2, firstValue, direction,
      Vector.fill(sideLength, sideLength)(init))
  }

  def nextDirection(sideLength: Int, x: Int, y: Int,
                    direction: Direction.Value, spiral: Vector[Vector[Long]]): Direction.Value =
    direction match {
      case Direction.R => if (x >= sideLength - 1 || !initialized(spiral(y + 1)(x))) Direction.U else Direction.R
      case Direction.D => if (y <= 0 || !initialized(spiral(y)(x + 1))) Direction.R  else Direction.D
      case Direction.L => if (x <= 0 || !initialized(spiral(y - 1)(x))) Direction.D else Direction.L
      case Direction.U => if (y >= sideLength - 1 || !initialized(spiral(y)(x - 1))) Direction.L else Direction.U
  }

  def nextX(thisX: Int, direction: Direction.Value): Int = {
    direction match {
      case Direction.R => thisX + 1
      case Direction.L => thisX - 1
      case _ => thisX
    }
  }

  def nextY(thisY: Int, direction: Direction.Value): Int = {
    direction match {
      case Direction.U => thisY + 1
      case Direction.D => thisY - 1
      case _ => thisY
    }
  }
}

object Direction extends Enumeration {
  val R, D, L, U = Value

}