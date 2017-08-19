package com.msokoryansky.MathUtils

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object Integer {
  /**
    * @param i first integer in the stream
    * @return Stream of all integers starting with i
    */
  def ints[A: Integral](i: A): Stream[A] = i #:: ints(implicitly[Integral[A]].plus(i, implicitly[Integral[A]].one))
  /*
  def ints(i: BigInt): Stream[BigInt] = i #:: ints(i + 1)
  def ints(i: Long): Stream[Long] = i #:: ints(i + 1)
*/
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

  def factorial(i: Int): Long = {
    require(i >= 0, "Can only take factorials of positive integers")
    @tailrec def factorialAcc(i: Int, acc: Long): Long = if (i <= 1) acc else factorialAcc(i - 1, i * acc)
    factorialAcc(i, 1)
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
    number == number.toString.toList.map(_.asDigit).map(factorial).sum
  }

  def circulars(number: Int): List[Int] = {
    import com.msokoryansky.MathUtils.StringOps.StringUtilityOps
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
    val fixedPlaces = Subset.subsets((0 to numDigits).toSet).filterNot(_.isEmpty)
    fixedPlaces.flatMap(fps => {
      val digitCombos = (("1" + "0" * fps.size).toLong until ("10" + "0" * fps.size).toLong)
        .map(s => s.toString.substring(1))
      digitCombos.map(digitCombo => fps.toList.zip(digitCombo).toMap.map(ic => ic._1 -> ic._2.asDigit)
        .filterNot(ic => ic._1 == 0 && ic._2 == 0))
    })
  }
}

object IntegerOps {
  implicit class DigitOps(n: Int) {
    def mapDigits[B](f: (Int) ⇒ B): List[B] = n.toString.map(c => f(c.asDigit)).toList
    def pow(power: Int): Long = Math.pow(n, power).toLong
  }
}