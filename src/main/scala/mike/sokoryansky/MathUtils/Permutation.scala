package mike.sokoryansky.MathUtils

import scala.annotation.tailrec
import util.Properties

object Permutation {
  /**
    * Returns ith element (in alphabetical order) of lexicographic permutations of chars in set s
    * @param s set of characters to be permutated
    * @param i lexigraphic number of permutation to return
    * @return ith lexigraphic permutation
    */
  def lexicographicPermutation(s: Set[Char], i: Int): String = {
    import IntegerOps._
    /*
    The general idea is to use the fact that we know that because the order of permutations is alphabetical,
    the first (N-1)! permutations will start with first (alphabetically) element. Next (N-1)! will start with
    second element, etc.
     */
    @tailrec def lexicographicPermutationAcc(s: Set[Char], i: Int, acc: String): String = {
      require(i >= 0, "Can only return elements startign with number 0 in lexicographic sequence")
      require(i < s.size.factorial, "Provided set has fewer permutations than the permutation number requested")
      s.size match {
        case 0 => acc
        case n =>
          val partition = (n - 1).factorial
          val headElementIndex = i / partition
          val headElement = s.toArray.sortWith(_ < _)(headElementIndex)
          val tailIndex = i % partition
          val tailSet = s.filter(_ != headElement)
          lexicographicPermutationAcc(tailSet, tailIndex, acc + headElement)
      }
    }
    lexicographicPermutationAcc(s, i, "")
  }

  /**
    * Returns all permutations using all characters in specified list
    */
  def permutations(chars: Set[Char]): List[String] = {
    def permutationsAcc(chars: List[Char], acc: List[String]): List[String] = chars match {
      case none if none.isEmpty => acc
      case _ => chars.flatMap(c => permutationsAcc(chars.filter(_ != c),
                                                    if (acc.isEmpty) List(c.toString) else acc.map(s => s + c)))
    }
    permutationsAcc(chars.toList, List[String]())
  }

  /**
    * Generic permutation of a set of As
    */
  def permutations2[A](as: Set[A]): List[List[A]] = {
    def permutationsAcc(as: List[A], acc: List[List[A]]): List[List[A]] = as match {
      case none if none.isEmpty => acc
      case _ => as.flatMap(a => permutationsAcc(as.filter(_ != a),
        if (acc.isEmpty) List(List(a)) else acc.map(s => a :: s)))
    }
    permutationsAcc(as.toList, List[List[A]]())
  }

  /**
    * Returns all permutations of characters of a string
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
}

/**
  * Exploring how integers can be composed as a sum of a specific set of smaller integers
  */
object SumsOfParts {
  /**
    * Finds all ways that pre-defined parts can add up to sum.
    *
    * E.g. Support that sum is 1 dollar and predefined parts are penny, nickel, dime, quarter, half-dollar.
    * generate will return a set of maps where each map holds number of coins for each coin denomination
    * so that each map's total value is 1 dollar.
    *
    * In this example, sum is 100 and parts are Set(1, 5, 10, 25, 50)
    */
  def generate(sum: Int, parts: Set[Int]): Set[Map[Int, Int]] = {
    def generateAcc(sum: Int, lowerstPart: Int, currentParts: Map[Int, Int], acc: Set[Map[Int, Int]]): Set[Map[Int, Int]] = {
      parts.filter(p => p >= lowerstPart && p == sum).map(p => currentParts.updated(p, currentParts(p) + 1)) union
       parts.filter(p => p >= lowerstPart && p < sum)
         .flatMap(p => generateAcc(sum - p, p, currentParts.updated(p, currentParts(p) + 1), acc))
    }
    generateAcc(sum, 0, parts.zip(List.fill(parts.size)(0)).toMap, Set[Map[Int, Int]]())
  }

  def generateAsSumToString(r: Set[Map[Int, Int]]): String =
    r.toList.map(m =>
      m.filterNot(_._2 == 0).toList.sortBy(_._1).reverse.map(t =>
        t._1.toString + ((" + " + t._1.toString) * (t._2 - 1))).mkString(" + "))
      .sorted.reverse.mkString(Properties.lineSeparator)

  def count(sum: Int, parts: Seq[Int]): Long = {
    def countAcc(s: Int, ps: Seq[Int]): Long = {
      if (s < 0 || ps.isEmpty) 0
      else if (s == 0 || (ps.size == 1 && s % ps.head == 0)) 1
      else ps.filter(_ <= s).map(p => p -> countAcc(s - p, ps.filter(_ <= p))).toMap.values.sum
    }
    parts.filter(_ < sum).map(p => countAcc(sum - p, parts.filter(_ <= p))).sum
  }


  /**
    * Similar to count but parts are all integers from 1 until sum.
    * Use StarsAndBars for a much, much better implementation of this functionality.
    */
  def count2(sum: Int): Long = {
    def count2Acc(s: Int, max: Int): Long = {
      if (s < 0) 0
      else if (s == 0 || max == 1) 1
      else (1 to max).map(p => count2Acc(s - p, p)).sum
    }
    (1 until sum).map(p => count2Acc(sum - p, p)).sum
  }
}
