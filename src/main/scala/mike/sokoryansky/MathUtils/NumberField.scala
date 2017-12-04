package mike.sokoryansky.MathUtils

import scala.util.{Properties, Random}

/**
  * Class representing a two-dimensional "field" of integers.
  * Each element of the inner array is a row in the field.
  * @param field represents two-dimension field of numbers.
  */
class NumberField (field: Seq[Seq[Int]]) {
  require(NumberField.isValid(field), "Invalid number field specified")
  val height: Int = field.length
  val width: Int = field.head.length
  val elSize: Int =
    field.map(row => row.max).max.toString.length +  (if (field.exists(_.exists(_ < 0))) 1 else 0)

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
    * Find a value for the given path.
    * For now it's simply sum of all nodes along that path
    */
  def evalPath(path: List[(Int, Int)]): Int = path.foldLeft(0)((e, cell) => e + el(cell._1, cell._2))

  /**
    * Brute force creation of all possible paths from top left corner to bottom right corner.
    *
    * We know that any such path must have (Height - 1) steps down and (Width - 1) steps to the right.
    * We use that fact to create "binary" representations of all such paths where
    * 0 is a step down and 1 is a step to the right.
    *
    * So we start by creating all permutations of (Height - 1) 0s and (Width - 1) 1s
    * and then map each one to an actual path.
    *
    * Fun observation
    * ---------------
    * Since number of "down steps" is (Height - 1) and number of "right steps" is (Width - 1),
    * the total number of steps -- call it NumSteps == (Width + Height - 2).
    *
    * By definition:  (Width - 1) == NumSteps - (Height - 1)
    * and vice versa: (Height - 1) == NumSteps - (Width - 1)
    *
    * Since we just essentially described all permutations of X ones -- or (NumSteps - X) zeros -- as
    * equalling the number of all possible paths, it's trivially true that number of possible paths is:
    *
    * NumSteps choose (Width - 1) == NumSteps choose (Height - 1) == (NumSteps! / ((Width - 1)! * (Height - 1)!)
    */
  def allPathsBruteForce: List[List[(Int, Int)]] = {
    /**
      * Returns all permutations of characters of a string.
      * Used to create "binary" representation of all possible paths.
      */
    def permutations(string: String): List[String] = {
      def permutationsAcc(string: String, acc: List[String]): List[String] = string match {
        case none if none.isEmpty => acc
        case _ => (0 until string.length).flatMap(i => {
          val char = string(i).toString
          val remaining = string.substring(0, i) + string.substring(i + 1)
          permutationsAcc(remaining, if (acc.isEmpty) List(char) else acc.map(s => s + char))
        }).toList
      }
      permutationsAcc(string, List[String]()).distinct
    }

    /**
      * Convert a binary representation of a path to the actual list of (x,y) coordinates in that path
      */
    def pathBin2Real(binary: String): List[(Int, Int)] = {
      def pathBin2RealAcc(x: Int, y: Int, bin: String, acc: List[(Int, Int)]): List[(Int, Int)] = {
        if (bin.isEmpty) acc
        else if (bin.startsWith("0")) pathBin2RealAcc(x, y + 1, bin.substring(1), (x, y + 1) :: acc)
        else pathBin2RealAcc(x + 1, y, bin.substring(1), (x + 1, y) :: acc)
      }
      pathBin2RealAcc(0, 0, binary, List((0, 0))).reverse
    }
    permutations("0" * (height - 1) + "1" * (width - 1)).map(bin => pathBin2Real(bin))
  }

  def bestPathBruteForce: List[(Int, Int)] = {
    allPathsBruteForce.maxBy(evalPath)
  }

  /**
    * Best path from any point (x, y) (defaulting top left corner, aka (0, 0)) to lower right corner.
    * Correct but very inefficient.
    */
  def bestPathSlow(x: Int = 0, y: Int = 0): List[(Int, Int)] = {
    def bestPathSlowAcc(x: Int, y: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      val accNext = (x, y) :: acc
      (x, y) match {
        case (right, bottom) if right == width - 1 && bottom == height - 1 => accNext
        case (right, other) if right == width - 1 => bestPathSlowAcc(x, y + 1, accNext)
        case (other, bottom) if bottom == height - 1 => bestPathSlowAcc(x + 1, y, accNext)
        case _ =>
          val overPath = bestPathSlowAcc(x + 1, y, accNext)
          val downPath = bestPathSlowAcc(x, y + 1, accNext)
          if (evalPath(overPath) > evalPath(downPath)) overPath else downPath
      }
    }
    bestPathSlowAcc(0, 0, List()).reverse
  }

  /**
    * Optimized best path which is simply a lookup in allBestPaths
    */
  def bestPath(x: Int = 0, y: Int = 0): List[(Int, Int)] = allBestPaths(x, y)
  def bestPathMin(x: Int = 0, y: Int = 0): List[(Int, Int)] = allBestPathsMin(x, y)

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
    * All best paths from any point (x, y) to lower right corner, by lowest path value. Lazily evaluated
    */
  lazy val allBestPathsMin: Map[(Int, Int), List[(Int, Int)]] = {
    def allBestPathsAcc(x: Int, y: Int, acc:Map[(Int, Int), List[(Int, Int)]]):Map[(Int, Int), List[(Int, Int)]] = {
      if (acc.contains((x, y))) acc
      else (x, y) match {
        case (right, bottom) if right == width - 1 && bottom == height - 1 => acc ++ Map((x, y) -> List((x, y)))
        case _ =>
          val acc2 = if (isEl(x, y + 1)) allBestPathsAcc(x, y + 1, acc) else acc
          val acc3 = if (isEl(x + 1, y)) allBestPathsAcc(x + 1, y, acc2) else acc2
          acc3 ++ Map((x, y) -> ((x, y) ::
            (if (isEl(x + 1, y) && isEl(x, y + 1))
              if (evalPath(acc3(x + 1, y)) < evalPath(acc3(x, y + 1))) acc3(x + 1, y) else acc3(x, y + 1)
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
    * Create NumberField from sequence of integer sequences
    */
  def apply(field: Seq[Seq[Int]]) = new NumberField(field)

  /**
    * Create NumberField from its string representation
    */
  def apply(field: String) = new NumberField(fromString(field))

  /**
    * Create a random x by y NumberField using specified set of numbers for values
    */
  def apply(width: Int, height: Int, numbers: Set[Int]): NumberField = {
    val nums = numbers.toVector
    val field = (0 until height).map(_ => (0 until width).map(_ => nums(Random.nextInt(nums.length))))
    new NumberField(field)
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