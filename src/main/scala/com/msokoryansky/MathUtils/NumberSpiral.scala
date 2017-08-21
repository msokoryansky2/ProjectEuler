package com.msokoryansky.MathUtils

import com.msokoryansky.MathUtils

import scala.annotation.tailrec

class NumberSpiral private(spiral: Vector[Vector[Long]]) {
  val sideLength: Int = spiral.length
  require(sideLength > 0 && sideLength % 2 == 1, "Number spiral side length must be an odd positive number")

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

  def trim(trim: Int): NumberSpiral = {
    require(trim >= 0, "Cannot specify negative trim")
    require(trim <= (sideLength - 1) / 2, "Specified trim would result in removing the entire the spiral")
    NumberSpiral(spiral.dropRight(trim).drop(trim).map(v => v.dropRight(trim).drop(trim)))
  }
}

object NumberSpiral {
  val init: Long = -999999999

  def initialized(n: Long): Boolean = n != init

  def apply (spiral: Vector[Vector[Long]]): NumberSpiral = new NumberSpiral(spiral)

  def apply(sideLength: Int,
            firstValue: Int = 1,
            direction: Direction.Value = Direction.R,
            clockwise: Clockwise.Value = Clockwise.CW): NumberSpiral = {
    require(sideLength > 0 && sideLength % 2 == 1, "Number spiral side length must be an odd positive number")
    new NumberSpiral(NumberSpiral.fillSpiral(sideLength, firstValue, direction, clockwise))
  }

  def fillSpiral(sideLength: Int,
                 firstValue: Int = 1,
                 direction: Direction.Value = Direction.R,
                 clockwise: Clockwise.Value = Clockwise.CW): Vector[Vector[Long]] = {
    @tailrec def fillSpiralAcc(x: Int, y: Int, value: Long, direction: Direction.Value,
                               acc: Vector[Vector[Long]]): Vector[Vector[Long]] = {
      if (x < 0 || y < 0 || x >= sideLength || y >= sideLength || initialized(acc(y)(x))) acc
      else {
        // a little ugly if this is the first element in the spiral -- in that case we always go in specified direction.
        val nextDir = if (x == (sideLength - 1) / 2 && x == y) direction
                      else nextDirection(sideLength, x, y, direction, clockwise, acc)
        fillSpiralAcc(nextX(x, nextDir), nextY(y, nextDir), value + 1, nextDir,
          acc.updated(y, acc(y).updated(x, value)))
      }
    }
    fillSpiralAcc((sideLength - 1) / 2, (sideLength - 1) / 2, firstValue, direction,
      Vector.fill(sideLength, sideLength)(init))
  }

  def nextDirection(sideLength: Int, x: Int, y: Int,
                    direction: Direction.Value,
                    clockwise: Clockwise.Value,
                    spiral: Vector[Vector[Long]]): Direction.Value =
    clockwise match {
      case Clockwise.CW => direction match {
        case Direction.R => if (x >= sideLength - 1 || !initialized(spiral(y + 1)(x))) Direction.U else Direction.R
        case Direction.D => if (y <= 0 || !initialized(spiral(y)(x + 1))) Direction.R else Direction.D
        case Direction.L => if (x <= 0 || !initialized(spiral(y - 1)(x))) Direction.D else Direction.L
        case Direction.U => if (y >= sideLength - 1 || !initialized(spiral(y)(x - 1))) Direction.L else Direction.U
      }
      case Clockwise.CCW => direction match {
        case Direction.R => if (x >= sideLength - 1 || !initialized(spiral(y - 1)(x))) Direction.D else Direction.R
        case Direction.D => if (y <= 0 || !initialized(spiral(y)(x - 1))) Direction.L else Direction.D
        case Direction.L => if (x <= 0 || !initialized(spiral(y + 1)(x))) Direction.U else Direction.L
        case Direction.U => if (y >= sideLength - 1 || !initialized(spiral(y)(x + 1))) Direction.R else Direction.U
      }
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

  object Direction extends Enumeration {
    val R, D, L, U = Value
  }

  object Clockwise extends Enumeration {
    val CW, CCW = Value
  }

  /**
    * Finds smallest matching spiral matching specified specs and predicate by iterating through possible spirals.
    * Allow to give up after maxSideLength iterations.
    */
  def find(firstValue: Int = 1,
           direction: Direction.Value = Direction.R,
           clockwise: Clockwise.Value = Clockwise.CW,
           p: (NumberSpiral) => Boolean,
           maxSideLength: Int = 0): Option[NumberSpiral] = {
    @tailrec def findAcc(startSideLength: Int,
                          stopSideLength: Int,
                          current: NumberSpiral,
                          acc: Option[NumberSpiral]): Option[NumberSpiral] = {
      if (p(current)) {
        if (current.sideLength <= stopSideLength) Some(current)
        else findAcc(startSideLength, stopSideLength, current.trim(1), Some(current))
      }
      else {
        if (current.sideLength <= stopSideLength) {
          if (acc.nonEmpty || (maxSideLength > 0 && startSideLength >= maxSideLength)) acc
          else {
            val nextStartSideLength = if (maxSideLength > 0) Math.min(startSideLength + 100, maxSideLength)
                                      else startSideLength + 100
            findAcc(nextStartSideLength, startSideLength,
              NumberSpiral(nextStartSideLength, firstValue, direction, clockwise), None)
          }
        } else {
          findAcc(startSideLength, stopSideLength, current.trim(1), acc)
        }
      }
    }
    val startSideLength = if (maxSideLength > 0) Math.min(101, maxSideLength) else 101
    findAcc(startSideLength, 1, NumberSpiral(startSideLength, firstValue, direction, clockwise), None)
  }

  /**
    * Stream of corner numbers of all number spirals for specified starting side.
    * Spiral with sidelength of 1 has only 1 "corner".
    * All consecutive spirals have 4 corners. Effectively this function returns all numbers in two main diagonals
    * of a spiral
    */
  def corners(side: Int = 1): Stream[(Int, Seq[Long])] =
    (if (side == 1) (1, Vector(1.toLong)) else (side, (0.toLong to 3).map(side * side - _ * (side - 1)))) #:: corners(side + 2)

  def cornersFind(p: Seq[Long] => Boolean, mixSideLength: Int): Seq[Long] = {
    def cornersFindAcc(s: Stream[(Int, Seq[Long])], acc: Seq[Long]): Seq[Long] = {
      val acc2 = s.head._2 ++ acc
      if (s.head._1 >= mixSideLength && p(acc2)) acc2 else cornersFindAcc(s.tail, acc2)
    }
    cornersFindAcc(corners(), Seq())
  }

  def cornersFind(p: Seq[Long] => Boolean,  mixSideLength: Int, maxSideLength: Int): Option[Seq[Long]] = {
    def cornersFindAcc(s: Stream[(Int, Seq[Long])], acc: Seq[Long]): Option[Seq[Long]] = {
      if (s.head._1 > maxSideLength) None
      else {
        val acc2 = s.head._2 ++ acc
        if (s.head._1 >= mixSideLength && p(acc2)) Some(acc2) else cornersFindAcc(s.tail, acc2)
      }
    }
    cornersFindAcc(corners(), Seq())
  }
}
