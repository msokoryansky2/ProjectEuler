package mike.sokoryansky.MathUtils

object Reciprocal {
  /**
    * Returns result of 1 / d in the form of two strings: first one is non-repeating digits after the decimal,
    * second is the repeating portion.
    * E.g.:
    * 1 / 5 should yield ("2", "") representing 0.2
    * 1 / 6 should yield ("1", "6") representing 0.1666666...
    * 1 / 7 should yield ("", "142857") representing 0.142857142857142857...
    * @param d integer to take reciprocal of
    */
  def reciprocal(d: Int): Decimal = {
    require(d > 0, "Only positive reciprocals supported")
    if (d == 1) Decimal(1, "", "")
    else {
      /**
        * @param carry carry from last place
        * @param acc accumulator for every decimal place of whole value for that place and carry into next place
        */
      def reciprocalAcc(carry: Int, acc: List[(Int, Int)]): Decimal = {
        val whole = carry * 10 / d
        val nextCarry = carry * 10 % d
        nextCarry match {
          case nc if nc == 0 => Decimal(0, ((whole, 0) :: acc).map(n => n._1.toString).mkString.reverse, "")
          case _ =>
            // Check if nextCarry has already occurred in acc
            val sameCarryIndex: Int = acc.indexWhere(_ == (whole, nextCarry))
            if (sameCarryIndex >= 0) {
              // If we are here that means it's a repeat of a carry which means we found the repeating sequence
              // Note that because acc is in reverse order of mathematical correct answer, the "repeats" come first
              // and the "start" comes after them. So we reverse the list first
              val (accS, accR) = acc.reverse.splitAt(acc.length - 1 - sameCarryIndex)
              Decimal(0, accS.map(n => n._1.toString).mkString, accR.map(n => n._1.toString).mkString)
            } else {
              // If we are here it's a new carry, which means we continue factoring
              reciprocalAcc(nextCarry, (whole, nextCarry) :: acc)
            }
        }
      }
      reciprocalAcc(1, List[(Int, Int)]())
    }
  }
}

/**
  * a way to represent a decimal fraction as a whole + digits immediately following the decimal point +
  * repeating digits following those immediately after the decimal point
  * @param whole          whole part of the fraction (may be 0)
  * @param dStart   non-repeating digits immediately following the decimal (may be "")
  * @param dRepeat    repeating digits following decimalStart (may be "")
  */
case class Decimal(whole: Int, dStart: String, dRepeat: String) {
  def this(d: Decimal) = this(d.whole, d.dStart, d.dRepeat)
  def this(s: String) = this(Decimal.fromString(s))

  override def toString: String = whole.toString +
    (if (dStart.length > 0 || dRepeat.length > 0) "." + dStart +
      (if (dRepeat.length > 0) "(" + dRepeat + ")" else "")
    else "")
}

object Decimal {
  def fromString(s: String): Decimal = {
    val wsr = "^([0-9]*)\\.([0-9]*)\\(([0-9]*)\\)$".r     // 123.45(678) or 0.45 or 0.45(678) or .45 or .45(678)
    val ws = "^([0-9]*)\\.([0-9]*)$".r                    // 123.45 or 0.45 or .45
    val w = "^([0-9]*)$".r                                // 123
    require(s.matches(w.regex) || s.matches(ws.regex) || s.matches(wsr.regex), "Invalid string format")
    s match {
      case wsr(mw, ms, mr) => Decimal(mw.toInt, ms, mr)
      case ws(mw, ms) => Decimal(mw.toInt, ms, "")
      case w(mw) => Decimal(mw.toInt, "", "")
    }
  }
}
