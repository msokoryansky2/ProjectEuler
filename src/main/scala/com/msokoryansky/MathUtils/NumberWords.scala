package com.msokoryansky.MathUtils

object NumberWords {
  def word(i: Long): String = i match {
    case n if n < 0 => "negative " + word(0 - i)
    case 0 => "zero"
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
    case 20 => "twenty"
    case 30 => "thirty"
    case 40 => "forty"
    case 50 => "fifty"
    case 60 => "sixty"
    case 70 => "seventy"
    case 80 => "eighty"
    case 90 => "ninety"
    case n if n > 20 && n < 100 && n % 10 != 0 => word(n - n % 10) + "-" + word(n % 10)
    case n if n > 0 && n < 1000 && n % 100 == 0 => word((n - n % 100) / 100) + " hundred"
    case n if n > 100 && n < 1000 =>
      word(n - n % 100) + (if (n % 100 != 0) " and " + word(n % 100) else "")
    case 1000 => "one thousand"
    case _ => ""
  }
}
