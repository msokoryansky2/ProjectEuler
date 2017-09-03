package mike.sokoryansky.MathUtils

import Ordering.Implicits._

object Subset {
  /**
    * Returns all possible subsets (including empty and self) of a specified set
    */
  def subsets[A: Ordering](set: Set[A]): Set[Set[A]] = {
    (0 to set.size).flatMap(size => subsets(set, size)).toSet
  }

  /**
    * Returns all possible subsets of specified length for specified set
    */
  def subsets[A: Ordering](set: Set[A], size: Int): Set[Set[A]] = {
    require(size >= 0 && size <= set.size, "Must specify subset size between 0 and size of set")
    size match {
      case n if n == 0 => Set(Set())
      case n if n == set.size => Set(set)
      case _ =>
        def subsetsAcc(setCurr: Set[A], sizeCurr: Int, acc: Set[Set[A]]): Set[Set[A]] = {
          if (sizeCurr >= size) acc
          else setCurr.filterNot(m => acc.exists(s => s.exists(_ > m))).flatMap(m => subsetsAcc(setCurr.filterNot(_ == m), sizeCurr + 1, acc.map(_ + m)))
        }
        set.flatMap(m => subsetsAcc(set.filterNot(_ == m), 1, Set(Set(m))))
    }
  }

  def waysToSelectRFromN(n: Int, r: Int): BigInt = {
    require(n > 0, "Number of elements must be positive")
    require(r > 0, "Number of elements selected must be positive")
    require(r <= n, "Number of elements selected must not exceed number of elements in the set")
    (BigInt(1) to n).product / ((BigInt(1) to r).product * (BigInt(1) to (n - r)).product)
  }
}
