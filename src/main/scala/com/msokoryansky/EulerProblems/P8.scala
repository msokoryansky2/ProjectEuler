package com.msokoryansky.EulerProblems

import scala.annotation.tailrec

class P8 extends EulerProblem {
  /**
    * Returns greatest product of numDigits consecutive digites in the number represented by a String
    * @param number a string representation of a arbitrarily large number
    * @param numDigits number of consecutive digits to consider for a product
    * @return greatest possible product of consecutive digits
    */
  def greatestProduct(number: String, numDigits: Int): BigInt = {
    @tailrec def greatestProductAcc(remaining: String, acc: BigInt): BigInt = {
      if (numDigits < 1 || remaining.length < numDigits) acc
      else greatestProductAcc(remaining.substring(numDigits),
        BigDecimal(Math.max(remaining.substring(0, numDigits).toList.map(_.toString.toInt).product.toDouble, acc.toDouble)).toBigInt)
    }
    greatestProductAcc(number, 0)
  }

  def run = greatestProduct("1234567876549988", 2).toString
}


object P8 extends App {
  (new P8).printAnswer
}