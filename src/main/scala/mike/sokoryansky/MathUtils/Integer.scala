package mike.sokoryansky.MathUtils

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object Integer {
  /**
    * @param i first integer in the stream
    * @return Stream of all integers starting with i
    */
  def ints[A: Integral](i: A): Stream[A] = i #:: ints(implicitly[Integral[A]].plus(i, implicitly[Integral[A]].one))

  /**
    * Stream of f(x) values for integral x
    */
  def ints[A: Integral](i: A, f: (A) => (A)): Stream[A] =
    f(i) #:: ints(implicitly[Integral[A]].plus(i, implicitly[Integral[A]].one), f)

  def intsDesc(hi: BigInt, lo: BigInt): Stream[BigInt] = if (hi >= lo) hi #:: intsDesc(hi - 1, lo) else Stream.Empty

  /**
    * @param i first integer to consider
    * @param limit cutoff at or above which we stop
    * @param include function to decide which integers should be included in the sum
    * @return Sum of all integers starting from i and less than limit that meet the include condition
    */
  def intsSum(i: Int, limit: Int, include: Int => Boolean): Int = {
    @tailrec def intsSumAcc(ints: Stream[Int], sum: Int): Int = {
      if (ints.head >= limit) sum
      else intsSumAcc(ints.tail, sum + (if (include(ints.head)) ints.head else 0))
    }
    intsSumAcc(ints(i), 0)
  }

  /*
  def factorial(i: Int): Long = {
    require(i >= 0, "Can only take factorials of positive integers")
    @tailrec def factorialAcc(i: Int, acc: Long): Long = if (i <= 1) acc else factorialAcc(i - 1, i * acc)
    factorialAcc(i, 1)
  }
  */

  def sumDigitsFactorial(n: Long): Long = {
    require(n >= 0, "Must specify  integer >= zero")
    val digiFacts = Map(0 -> 1, 1 -> 1, 2 -> 2, 3 -> 6, 4 -> 24, 5 -> 120, 6 -> 720, 7 -> 5040, 8 -> 40320, 9 -> 362880)
    n.toString.toList.map(d => digiFacts(d.asDigit)).sum
  }

  /**
    * A chain of sums of digits' factorials goes until a repeat. E.g.: 78 → 45360 → 871 → 45361 (→ 871)
    * In the above example, sumDigitsFactorialChainLength(78) should return 4 (length of Seq(78, 45360, 871, 45361))
    */
  def sumDigitsFactorialChainLength(n: Long): Long = {
    require(n >= 0, "Must specify  integer >= zero")
    def sumDigitsFactorialChainLengthAcc(i: Long, acc: HashSet[Long]): Long = {
      val next = sumDigitsFactorial(i)
      if (acc.contains(next)) acc.size
      else sumDigitsFactorialChainLengthAcc(next, acc ++ HashSet(next))
    }
    sumDigitsFactorialChainLengthAcc(n, HashSet[Long](n))
  }

  /**
    * sumDigitsFactorialChainLength for all integers from 1 to N, optimized to re-use already known chain lengths
    */
  def sumDigitsFactorialChainLength1ToN(n: Long): Map[Long, Long] = {
    require(n >= 1, "Must specify  integer >= 1")
    def sumDigitsFactorialChainLength1ToNAcc(local: Long,
                                             accLocal: HashSet[Long],
                                             n2: Long,
                                             acc: Map[Long, HashSet[Long]]): Map[Long, Long] = {
      if (n2 > n) acc.map(nc => nc._1 -> nc._2.size.toLong)
      else {
        val next = sumDigitsFactorial(local)
        if (acc.contains(next) /* && accLocal.intersect(acc(next)).isEmpty *** Read below why this is not needed ***/ )
          // The reasons we do not need to check for set intersection is because our ultimate chain length for n2
          // will be set union of the imcomplete local chain (accLocal) +
          // already known chain for next element of current chain (acc(next)).
          //
          // It's very possible for accLocal to contain an element that occurs in acc(next). E.g.:
          // acc(169) has length 3: 169 -> 363601-> 1454 (next would be 169 -- a repeat)
          // accLocal for 174: 174 -> 5065 -> 961 -> 363601 -> 1454 (next would be 169 -- a match in acc)
          // However element 363601 of accLocal matches an element of acc(169), meaning that a hypothetical full chain
          // for 174 would be:  174 -> 5065 -> 961 -> 363601 -> 1454 -> 169 (next would be 363601 -- a repeat).
          // However the union operation of accLocal ++ acc(169) produces the same set of numbers since everything post-
          // 363601 repeats in both sets. That's why we don't need to worry about checking for intersection
          sumDigitsFactorialChainLength1ToNAcc(n2 + 1,
                                                HashSet[Long](n2 + 1),
                                                n2 + 1,
                                                acc + (n2 -> (accLocal ++ acc(next))))
        else {
          if (accLocal.contains(next))
            sumDigitsFactorialChainLength1ToNAcc(n2 + 1,
                                                  HashSet[Long](n2 + 1),
                                                  n2 + 1,
                                                  acc + (n2 -> accLocal))
          else
            sumDigitsFactorialChainLength1ToNAcc(next,
                                                  accLocal ++ HashSet(next),
                                                  n2,
                                                  acc)
        }
      }
    }
    sumDigitsFactorialChainLength1ToNAcc(1, HashSet[Long](1), 1, Map[Long, HashSet[Long]]())
  }

  /**
    * Returns greatest product of numDigits consecutive digits in the number represented by a String
    * @param number    a string representation of a arbitrarily large number
    * @param numDigits number of consecutive digits to consider for a product
    * @return greatest possible product of consecutive digits
    */
  def greatestProduct(number: String, numDigits: Int): Long = {
    @tailrec def greatestProductAcc(remaining: String, acc: Long): Long = {
      if (numDigits < 1 || remaining.length < numDigits) acc
      else greatestProductAcc(remaining.substring(1),
        Math.max(remaining.substring(0, numDigits).toList.map(_.toString.toLong).product, acc))
    }
    greatestProductAcc(number, 0)
  }

  def divisors(number: Long): HashSet[Long] = {
    require(number > 0, "Must specify a positive integer")
    @tailrec def divisorsAcc(next: Long, ceiling: Long, acc: HashSet[Long]): HashSet[Long] = {
      if (next > ceiling) acc
      else {
        if (number % next > 0) divisorsAcc(next + 1, ceiling, acc)
        else {
          val factor1 = next
          val factor2 = number / next
          val acc1 = if (acc.contains(factor1)) acc else acc + factor1
          val acc2 = if (acc1.contains(factor2)) acc1 else acc1 + factor2
          divisorsAcc(factor1 + 1, factor2, acc2)
        }
      }
    }
    divisorsAcc(1, number, HashSet[Long]())
  }

  def isSumOf2Elements(number: Long, elements: Set[Long]): Boolean = {
    elements.exists(e1 => elements.contains(number - e1))
  }

  def isSumDigitFactorials(number: Int): Boolean = {
    import IntegerOps._
    number == number.toString.toList.map(_.asDigit).map(_.factorial).sum
  }

  def circulars(number: Int): List[Int] = {
    import mike.sokoryansky.MathUtils.StringOps.StringUtilityOps
    val numberString = number.toString
    (0 until numberString.length).map(numberString.rotate).map(_.toInt).toList
  }

  def base2(number: Long): String = {
    require(number >= 0, "Cannot convert negative numbers to base 2")
    if (number == 0) "0"               // 0 is easier to take care of separately
    else {
      val numBits = Math.ceil(Math.log(number) / Math.log(2) + 1).toInt
      def base2Acc(number: Long, power: Int, acc: String): String = {
        power match {
          case neg if neg < 0 => acc
          case _ =>
            val bitValue = Math.pow(2, power).toInt
            if (number >= bitValue) base2Acc(number - bitValue, power - 1, acc + "1")
            else base2Acc(number, power - 1, if (acc.isEmpty) acc else acc + "0")
        }
      }
      base2Acc(number, numBits, "")
    }
  }

  def trimsRight(number: Long): List[Long] = {
    require(number >= 0, "Can only trim positive numbers")
    @tailrec def trimsRightAcc(number: Long, acc: List[Long]): List[Long] =
      if (number == 0) acc else trimsRightAcc(number / 10, number :: acc)
    trimsRightAcc(number / 10, List(number))
  }

  def trimsLeft(number: Long): List[Long] = {
    require(number >= 0, "Can only trim positive numbers")
    @tailrec def trimsLeftAcc(power: Long, acc: List[Long]): List[Long] = {
      val tenToPower = Math.pow(10, power).toLong
      if (tenToPower > number) acc
      else trimsLeftAcc(power + 1, if (acc.contains(number % tenToPower)) acc else (number % tenToPower) :: acc)
    }
    trimsLeftAcc(1, List(number))
  }

  /**
    * Returns all possible subsets of a numDigits-digit number where each subset specifies digit number and its value.
    * E.g.  Map(0 -> 5, 1 -> 6, 4 -> 3) represents numbers 56xx3. We return all possible non-empty subsets with
    * each subset having all possible digit permutations.
    */
  def subsetsWithFixedDigits(numDigits: Int): Set[Map[Int, Int]] = {
    val digitSubsets = Subset.subsets((0 until numDigits).toSet)
    digitSubsets.flatMap(ds => {
      val combos = (("1" + "0" * ds.size).toLong until ("2" + "0" * ds.size).toLong).map(s => s.toString.substring(1))
      combos.map(combo => ds.toList.zip(combo.map(_.asDigit)).filterNot(ic => ic._1 == 0 && ic._2 == 0).toMap)
    }).filterNot(_.isEmpty)
  }

  /**
    * Looks if number can be broken into concatenation of two parts.
    * @param number  integer to be decatenate (as in de-con-catenate)
    * @param parts   parts into which integer can be de-catenated
    * @return        list of pairs of parts that can be concatenated back to number
    */
  def decatenate(number: Long, parts: IndexedSeq[Long]): Seq[(Long, Long)] = {
    for {
      n <- 1 until number.toString.length
      (str1, str2) = number.toString.splitAt(n)
      if !str2.startsWith("0") && parts.contains(str1.toLong) &&  parts.contains(str2.toLong)
    } yield (str1.toLong, str2.toLong)
  }

  /**
    * All permutations of digits of a number (excludes "numbers" starting with zero
    */
  def permutations(number: Long): List[Long] =
    Permutation.permutations(number.toString).filterNot(_.startsWith("0")).map(_.toLong)

  /**
    * Check if one number is digit-permutation of another
    */
  def isPermutation(number1: Long, number2: Long): Boolean =
    number1.toString.toList.sortWith(_ < _) == number2.toString.toList.sortWith(_ < _)

  /**
    * Check if number is a specified power of some whole number
    */
  def isPow(number: Long, power: Double): Boolean = {
    val v = Math.pow(number, 1 / power)
    v.isWhole || Math.pow(v.floor, power) == number.toDouble ||  Math.pow(v.ceil, power) == number.toDouble
  }

  /**
    * Greatest common divisor of two numbers
    */
  @tailrec def gcd(a: Long, b: Long): Long = {
    require(a > 0 && b > 0, "Must specify two positive integers")
    if (b > a) gcd(b, a)
    else if (a % b == 0) b
    else gcd(b, a % b)
  }
}

object IntegerOps {
  implicit class DigitOps[A: Integral](n: A) {
    def mapDigits[B](f: (Int) ⇒ B): List[B] = n.toString.map(c => f(c.asDigit)).toList
    def pow(power: Int): Long = Math.pow(implicitly[Integral[A]].toDouble(n), power).toLong
    def sumDigits: A = {
      @tailrec def sumDigitsAcc(tail: A, acc: A): A = {
        if (tail == implicitly[Integral[A]].fromInt(0)) acc
        else sumDigitsAcc(implicitly[Integral[A]].quot(tail, implicitly[Integral[A]].fromInt(10)),
          implicitly[Integral[A]].plus(acc, implicitly[Integral[A]].rem(tail,
            implicitly[Integral[A]].fromInt(10))))
      }
      sumDigitsAcc(n, implicitly[Integral[A]].fromInt(0))
    }
    def factorial: A = {
      require(implicitly[Integral[A]].gteq(n, implicitly[Integral[A]].zero), "Factorial requires integer >= 0")
      @tailrec def fAcc(i: A, acc: A): A =
        if (implicitly[Integral[A]].lteq(i, implicitly[Integral[A]].one)) acc
        else fAcc(implicitly[Integral[A]].minus(i, implicitly[Integral[A]].one), implicitly[Integral[A]].times(i, acc))
      fAcc(n, implicitly[Integral[A]].one)
    }
  }
}