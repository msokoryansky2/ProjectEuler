package com.msokoryansky.MathUtils

object Calendar {
  /**
    * Return number of the specified day relative to 1 Jan 1900 which was a Monday with dayNumber of 0
    */
  def dayNumber(year: Int, month: Month.Value, day: Int): Long = {
    require(isValidDay(year, month, day), "Date $year - $month - $day is invalid")
    require(year >= 1900, "Year has to be 1900 or later")
    (1900 until year).toList.map(yearDays).sum                          // days in full preceding
    + Month.values.filter(_ < month).map(monthDays(year, _)).sum        // days in full month of that year
    + day
  }

  def isValidDay(year: Int, month: Month.Value, day: Int): Boolean = day >= 1 && day <= monthDays(year, month)

  def monthDays(year: Int, month: Month.Value): Int = month match {
    case d31 if List(Month.Jan, Month.Mar, Month.May, Month.Jul, Month.Aug, Month.Oct, Month.Dec) contains d31 => 31
    case d30 if List(Month.Apr, Month.Jun, Month.Sep, Month.Nov) contains d30 => 30
    case feb => if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0) 29 else 28
  }

  def yearDays(year: Int): Int = Month.values.map(monthDays(year, _)).sum
}

object Weekday extends Enumeration {
  val Mon = Value(1)
  val Tue = Value(2)
  val Wed = Value(3)
  val Thu = Value(4)
  val Fri = Value(5)
  val Sat = Value(6)
  val Sun = Value(7)
}

object Month extends Enumeration {
  val Jan = Value(1)
  val Feb = Value(2)
  val Mar = Value(3)
  val Apr = Value(4)
  val May = Value(5)
  val Jun = Value(6)
  val Jul = Value(7)
  val Aug = Value(8)
  val Sep = Value(9)
  val Oct = Value(10)
  val Nov = Value(11)
  val Dec = Value(12)
}
