package mike.sokoryansky.MathUtils

import scala.util.{Properties, Random}

// Different ways to evaluate path value (to a single Long) number
abstract class NFPathValue { def eval(path: List[Long]): Long }
class NFPathSum extends NFPathValue { def eval(path: List[Long]): Long = path.sum }
class NFPathProduct extends NFPathValue { def eval(path: List[Long]): Long = path.product }

// Different ways to compare which path (represented by a single Long value) is more fit
abstract class NFPathFitness { def isMoreFit(a: Long, b: Long): Boolean }
class NFMinPath extends NFPathFitness { override def isMoreFit(a: Long, b: Long): Boolean = a < b }
class NFMaxPath extends NFPathFitness { override def isMoreFit(a: Long, b: Long): Boolean = a > b }

// Different locations, used determine whether cell is a valid start or finish of the path
abstract class NFLoc { def is(field: NumberField, x: Int, y: Int): Boolean }
class NFLocU extends NFLoc { def is(field: NumberField, x: Int, y: Int): Boolean = x == 0 }
class NFLocUR extends NFLoc { def is(field: NumberField, x: Int, y: Int): Boolean = x == field.height - 1 && y == 0 }
class NFLocR extends NFLoc { def is(field: NumberField, x: Int, y: Int): Boolean = y == field.width - 1 }
class NFLocDR extends NFLoc { def is(field: NumberField, x: Int, y: Int): Boolean = x == field.height - 1 && y == field.width - 1 }
class NFLocD extends NFLoc { def is(field: NumberField, x: Int, y: Int): Boolean = x == field.height - 1 }
class NFLocDL extends NFLoc { def is(field: NumberField, x: Int, y: Int): Boolean = x == 0 && y == field.width - 1 }
class NFLocL extends NFLoc { def is(field: NumberField, x: Int, y: Int): Boolean = y == 0 }
class NFLocUL extends NFLoc { def is(field: NumberField, x: Int, y: Int): Boolean = x == 0 && y == 0 }

// Different directions that the path is allowed to turn within the field
object NFDir extends Enumeration {
  type dir = Value
  val U = Value(1, "U") // Up
  val R = Value(2, "R") // Right
  val D = Value(3, "D") // Down
  val L = Value(4, "L") // Left

  def dX(x: Int, dir: NFDir.Value): Int = if (dir == NFDir.R) x + 1 else if (dir == NFDir.L) x - 1 else x
  def dY(y: Int, dir: NFDir.Value): Int = if (dir == NFDir.D) y + 1 else if (dir == NFDir.U) y - 1 else y
}

/**
  * Class representing a two-dimensional "field" of integers.
  * Each element of the inner array is a row in the field.
  * @param field represents two-dimension field of numbers.
  */
class NumberField (val field: Seq[Seq[Int]],
                   val dirs: List[NFDir.dir],
                   val value: NFPathValue,
                   val fitness: NFPathFitness,
                   val start: NFLoc,
                   val finish: NFLoc) {
  require(NumberField.isValid(field), "Invalid number field specified")
  require(dirs.nonEmpty, "Must specify some allowed directions for the path to follow through the field")
  val height: Int = field.length
  val width: Int = field.head.length
  val elSize: Int =
    field.map(row => row.max).max.toString.length +  (if (field.exists(_.exists(_ < 0))) 1 else 0)

  def withDirs(newDirs: List[NFDir.dir]): NumberField = new NumberField(field, newDirs, value, fitness, start, finish)
  def withValue(newValue: NFPathValue): NumberField = new NumberField(field, dirs, newValue, fitness, start, finish)
  def withFitness(newFitness: NFPathFitness): NumberField = new NumberField(field, dirs, value, newFitness, start, finish)
  def withStart(newStart: NFLoc): NumberField = new NumberField(field, dirs, value, fitness, newStart, finish)
  def withFinish(newFinish: NFLoc): NumberField = new NumberField(field, dirs, value, fitness, start, newFinish)

  /**
    * Check if given coordinates of an element are valid for this field
    */
  def isEl(x: Int, y: Int): Boolean = y >= 0 && y < height && x >= 0 && x < width

  /**
    * Return number at specified coordinates
    */
  def el(x: Int, y: Int): Int = {
    require(isEl(x, y), s"Invalid co-ordinates ($x, $y) specified")
    field(y)(x)
  }

  /**
    * Convert path as List of coordinates to List of node values (to be passed onto NFPathValue evaluator)
    */
  def els(path: List[(Int, Int)]): List[Long] = path.map(cell => el(cell._1, cell._2).toLong)

  def all: Seq[(Int, Int)] = (0 until width).flatMap(x => (0 until height).map(y => (x, y)))

  /**
    * Best path overall which is best path from any of the starting cells
    */
  def bestPath: List[(Int, Int)] = {
    val bestPathFromEachStartCell: Seq[List[(Int, Int)]] = for {
      x <- 0 until width
      y <- 0 until height
      if start.is(this, x, y)
    } yield allBestPaths(x, y)
    // For each of valid best paths, find its value and then sort them by fitness (off that value), and return top one
    bestPathFromEachStartCell
      .map(p => (p, value.eval(els(p))))
      .sortWith((pv1, pv2) => fitness.isMoreFit(pv1._2, pv2._2))
      .head
      ._1
  }

  /**
    *  Best path from specified coordinates which is simply a lookup in allBestPaths
    */
  def bestPath(x: Int, y: Int): List[(Int, Int)] = allBestPaths(x, y)

  /**
    * All best paths from any point (x, y) to lower right corner. Lazily evaluated
    */
  lazy val allBestPaths2: Map[(Int, Int), List[(Int, Int)]] = {
    def allBestPathsAcc(x: Int,
                        y: Int,
                        visited: Map[(Int, Int), Boolean],
                        acc: Map[(Int, Int), List[(Int, Int)]]): Map[(Int, Int), List[(Int, Int)]] = {
      if (acc.contains((x, y))) acc
      else (x, y) match {
        case (pathFinished) if finish.is(this, x, y) => acc + ((x, y) -> List((x, y)))
        case _ =>
          // Calculate best paths for all allowed directions, making sure we re-use already calculated values
          var accNext = acc           // a var is really convenient here, sorry :(
          NFDir.values.filter(dirs.contains(_)).foreach { d =>
            val x2 = NFDir.dX(x, d)
            val y2 = NFDir.dY(y, d)
            if (isEl(x2, y2) && !visited.isDefinedAt(x2, y2))
              accNext = allBestPathsAcc(x2, y2, visited + ((x, y) -> true), accNext)
          }

          print(s"At ($x, $y) accNext is $accNext")

          // Compute path values in all valid directions using acc4 (illegal directions will be filtered out)
          val pathValues: List[(NFDir.Value, Long)] =
                List(NFDir.U, NFDir.R, NFDir.D, NFDir.L)
                  .filter(d => dirs.contains(d))                        // Only consider allowed directions
                  .filter(d => isEl(NFDir.dX(x, d), NFDir.dY(y, d)))    // Only consider a direction if it doesn't fall off the field
                  .filterNot(d => start.is(this, NFDir.dX(x, d), NFDir.dY(y, d)))     // Exclude other starting cells
                  .map(d => (d, value.eval(els(accNext(NFDir.dX(x, d), NFDir.dY(y, d))))))
          // Pick direction with most fit path value
          val bestDir: NFDir.Value = pathValues.sortWith((pv1, pv2) => fitness.isMoreFit(pv1._2, pv2._2)).head._1
          // Return path using best direction

          println(s" and best direction is $bestDir")

          accNext + ((x, y) -> ((x, y) :: accNext(NFDir.dX(x, bestDir), NFDir.dY(y, bestDir))))
      }
    }

    // Can't just default to (0, 0) starting point because we may not be allowed to go Down (or Right)
    val (xStart: Int, yStart: Int) = all.find(p => start.is(this, p._1, p._2)).get
    allBestPathsAcc(xStart, yStart, Map((xStart, yStart) -> true), Map())
  }

  lazy val allBestPaths: Map[(Int, Int), List[(Int, Int)]] = {
    // Trivial paths are finish cells containing paths to themselves
    val finishPaths: Map[(Int, Int), List[(Int, Int)]] =
      all.filter(p => finish.is(this, p._1, p._2)).map(p => p -> List(p)).toMap

    def allBestPathsAcc(x: Int,
                        y: Int,
                        visited: Map[(Int, Int), Boolean],
                        acc: Map[(Int, Int), List[(Int, Int)]]): Map[(Int, Int), List[(Int, Int)]] = {
      if (acc.contains((x, y))) acc
      else (x, y) match {
        case (pathFinished) if finish.is(this, x, y) => acc + ((x, y) -> List((x, y)))
        case _ =>
          // Calculate best paths for all allowed directions, making sure we re-use already calculated values
          var accNext = acc // a var is really convenient here, sorry :(
          NFDir.values.filter(dirs.contains(_)).foreach { d =>
            val x2 = NFDir.dX(x, d)
            val y2 = NFDir.dY(y, d)
            if (isEl(x2, y2) && !visited.isDefinedAt(x2, y2))
              accNext = allBestPathsAcc(x2, y2, visited + ((x, y) -> true), accNext)
          }

          //print(s"At ($x, $y) accNext is $accNext")

          // Compute path values in all valid directions using acc4 (illegal directions will be filtered out)
          val pathValues: List[(NFDir.Value, Long)] =
            List(NFDir.U, NFDir.R, NFDir.D, NFDir.L)
              .filter(d => dirs.contains(d)) // Only consider allowed directions
              .filter(d => isEl(NFDir.dX(x, d), NFDir.dY(y, d))) // Only consider a direction if it doesn't fall off the field
              .filterNot(d => start.is(this, NFDir.dX(x, d), NFDir.dY(y, d))) // Exclude other starting cells
              .map(d => (d, value.eval(els(accNext(NFDir.dX(x, d), NFDir.dY(y, d))))))
          // Pick direction with most fit path value
          val bestDir: NFDir.Value = pathValues.sortWith((pv1, pv2) => fitness.isMoreFit(pv1._2, pv2._2)).head._1
          // Return path using best direction

          //println(s" and best direction is $bestDir")

          accNext + ((x, y) -> ((x, y) :: accNext(NFDir.dX(x, bestDir), NFDir.dY(y, bestDir))))
      }
    }

    // Can't just default to (0, 0) starting point because we may not be allowed to go Down (or Right)
    val (xStart: Int, yStart: Int) = all.find(p => start.is(this, p._1, p._2)).get
    allBestPathsAcc(xStart, yStart, Map((xStart, yStart) -> true), finishPaths)
  }

  /**
    * Print a sinlge element. Optional predicate that if evaluated to false for (x, y) will print dots instead of number
    */
  def elToString(xy: (Int, Int), p: ((Int, Int)) => Boolean = _ => true): String = {
    val padder = "%" + elSize.toString + "s"
    if (p(xy)) padder.format(el(xy._1, xy._2)) else "." * elSize
  }

  /**
    * Print a path to string
    */
  def pathToString(path: List[(Int, Int)]): String =
    field.zipWithIndex.map(row =>
      row._1.zipWithIndex.map(col => elToString((col._2, row._2), path.contains(_)))
      .mkString(" ")).mkString(Properties.lineSeparator)

  /**
    * Print the entire field to string
    */
  def fieldToString: String = toString
  override def toString: String =
    field.zipWithIndex.map(row =>
      row._1.zipWithIndex.map(col => elToString((col._2, row._2)))
        .mkString(" ")).mkString(Properties.lineSeparator)
}

object NumberField {
  /**
    * Create NumberField from sequence of integer sequences with default eval params
    */
  def apply(field: Seq[Seq[Int]]) =
    new NumberField(field,
                    List(NFDir.D, NFDir.R),
                    new NFPathSum,
                    new NFMinPath,
                    new NFLocUL,
                    new NFLocDR)

  /**
    * Create NumberField from its string representation with default eval params
    */
  def apply(field: String) =
    new NumberField(fromString(field),
                    List(NFDir.D, NFDir.R),
                    new NFPathSum,
                    new NFMinPath,
                    new NFLocUL,
                    new NFLocDR)

  /**
    * Create a random x by y NumberField using specified set of numbers for values and default eval params
    */
  def apply(width: Int, height: Int, numbers: Set[Int]): NumberField = {
    val nums = numbers.toVector
    val field = (0 until height).map(_ => (0 until width).map(_ => nums(Random.nextInt(nums.length))))
    new NumberField(field,
                    List(NFDir.D, NFDir.R),
                    new NFPathSum,
                    new NFMinPath,
                    new NFLocUL,
                    new NFLocDR)
  }

  /**
    * Check if specified sequence of integer sequences creates a rectangular grid
    */
  def isValid(field: Seq[Seq[Int]]): Boolean =
    field.nonEmpty && field.head.nonEmpty && !field.exists(row => row.length != field.head.length)

  /**
    * Parse a string grid to sequence of integer sequences
    */
  def fromString(field: String): Seq[Seq[Int]] =
    field.split(Properties.lineSeparator).filterNot(_.trim.isEmpty).map(row => row.split("[\\s,]+").toSeq.map(_.toInt))
}

object NumberFieldApp extends App {
  val nf = NumberField(50, 50, (-10 to 20).toSet)
  val path = nf.bestPath

  println(nf + "\n\n\n")
  println(nf.pathToString(path))
}