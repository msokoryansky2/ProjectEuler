package mike.sokoryansky.MathUtils

import scala.util.{Properties, Random}

// Different ways to evaluate path value (to a single Long) number
abstract class NFPathValue {
  abstract def eval(path: List[Long]): Long
}
class NFPathSum extends NFPathValue {
  def eval(path: List[Long]): Long = path.sum
}
class NFPathProduct extends NFPathValue {
  def eval(path: List[Long]): Long = path.product
}

// Different ways to compare which path (represented by a single Long value) is more fit
abstract class NFPathFitness {
  abstract def isMoreFit(a: Long, b: Long): Boolean
}
class NFMinPath extends NFPathFitness {
  override def isMoreFit(a: Long, b: Long): Boolean = a < b
}
class NFMaxPath extends NFPathFitness {
  override def isMoreFit(a: Long, b: Long): Boolean = a > b
}

// Different ways to determine whether cell is start of the path
abstract class NFPathStart {
  abstract def isStart(field: NumberField, x: Int, y: Int): Boolean
}
class NFPathStartTopLeft extends NFPathStart {
  def isStart(field: NumberField, x: Int, y: Int): Boolean = x == 0 && y == 0
}
class NFPathLeftCol extends NFPathStart {
  def isStart(field: NumberField, x: Int, y: Int): Boolean = x == 0
}

// Different ways to determine whether cell is end of the path
abstract class NFPathFinish {
  abstract def isFinish(field: NumberField, x: Int, y: Int): Boolean
}
class NFPathFinishBottomRightCell extends NFPathFinish {
  def isFinish(field: NumberField, x: Int, y: Int): Boolean = x == field.width - 1 && y == field.height - 1
}
class NFPathFinishRightCol extends NFPathFinish {
  def isFinish(field: NumberField, x: Int, y: Int): Boolean = x == field.width - 1
}


// Different directions that the path is allowed to turn within the field
object NFDir extends Enumeration {
  type dir = Value
  val U = Value(1, "U") // Up
  val R = Value(2, "R") // Right
  val D = Value(3, "D") // Down
  val L = Value(5, "L") // Left
}

/**
  * Class representing a two-dimensional "field" of integers.
  * Each element of the inner array is a row in the field.
  * @param field represents two-dimension field of numbers.
  */
class NumberField (field: Seq[Seq[Int]],
                   dirs: List[NFDir.dir],
                   value: NFPathValue,
                   fitness: NFPathFitness,
                   start: NFPathStart,
                   finish: NFPathFinish) {
  require(NumberField.isValid(field), "Invalid number field specified")
  require(dirs.nonEmpty, "Must specify some allowed directions for the path to follow through the field")
  val height: Int = field.length
  val width: Int = field.head.length
  val elSize: Int =
    field.map(row => row.max).max.toString.length +  (if (field.exists(_.exists(_ < 0))) 1 else 0)

  def withDirs(newDirs: List[NFDir.dir]): NumberField = new NumberField(field, newDirs, value, fitness, start, finish)
  def withValue(newValue: NFPathValue): NumberField = new NumberField(field, dirs, newValue, fitness, start, finish)
  def withFitness(newFitness: NFPathFitness): NumberField = new NumberField(field, dirs, value, newFitness, start, finish)
  def withStart(newStart: NFPathStart): NumberField = new NumberField(field, dirs, value, fitness, newStart, finish)
  def withFinish(newFinish: NFPathFinish): NumberField = new NumberField(field, dirs, value, fitness, start, newFinish)

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

  /**
    * Optimized best path which is simply a lookup in allBestPaths
    */
  def bestPath(x: Int = 0, y: Int = 0): List[(Int, Int)] = allBestPaths(x, y)

  /**
    * All best paths from any point (x, y) to lower right corner. Lazily evaluated
    */
  lazy val allBestPaths: Map[(Int, Int), List[(Int, Int)]] = {
    def allBestPathsAcc(x: Int, y: Int, acc:Map[(Int, Int), List[(Int, Int)]]):Map[(Int, Int), List[(Int, Int)]] = {
      if (acc.contains((x, y))) acc
      else (x, y) match {
        case (right, bottom) if right == width - 1 && bottom == height - 1 => acc ++ Map((x, y) -> List((x, y)))
        case _ =>
          val acc2 = if (isEl(x, y + 1)) allBestPathsAcc(x, y + 1, acc) else acc
          val acc3 = if (isEl(x + 1, y)) allBestPathsAcc(x + 1, y, acc2) else acc2
          acc3 ++ Map((x, y) -> ((x, y) ::
            (if (isEl(x + 1, y) && isEl(x, y + 1))
              if (evalPath(acc3(x + 1, y)) > evalPath(acc3(x, y + 1))) acc3(x + 1, y) else acc3(x, y + 1)
            else if (isEl(x + 1, y)) acc3(x + 1, y)
            else acc3(x, y + 1))))
      }
    }
    allBestPathsAcc(0, 0, Map())
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
                    new NFPathStartTopLeft,
                    new NFPathFinishBottomRightCell)

  /**
    * Create NumberField from its string representation with default eval params
    */
  def apply(field: String) =
    new NumberField(fromString(field),
                    List(NFDir.D, NFDir.R),
                    new NFPathSum,
                    new NFMinPath,
                    new NFPathStartTopLeft,
                    new NFPathFinishBottomRightCell)

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
                    new NFPathStartTopLeft,
                    new NFPathFinishBottomRightCell)
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
  val path = nf.bestPath()

  println(nf + "\n\n\n")
  println(nf.pathToString(path))
}