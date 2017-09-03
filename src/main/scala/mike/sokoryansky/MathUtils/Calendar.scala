package mike.sokoryansky.MathUtils

object Calendar {
  val Year1900 = 1900

  /**
    * Return number of the specified day relative to 1 Jan 1900 which was a Monday with dayNumber of 1
    */
  def dayNumber(d: (Int, Month.Value, Int)): Long = dayNumber(d._1, d._2, d._3)
  def dayNumber(year: Int, month: Month.Value, day: Int): Long = {
    require(isValidDay(year, month, day), "Date $year - $month - $day is invalid")
    require(year >= 1900, "Year has to be 1900 or later")
    (1900 until year).toList.map(yearDays).sum  +                               // days in full preceding
      Month.values.toList.filter(_ < month).map(monthDays(year, _)).sum +       // days in full month of that year
      day
  }

  def day(number: Long): (Int, Month.Value, Int) = {
    require(number >= 1, "Day number must be positive")
    val yearGuess = Math.max(Year1900, Year1900 + (number / 365).toInt - 1)
    def yearsGuessAcc(year: Int, accDays: Long): Int =
      if (accDays + yearDays(year) >= number) year else yearsGuessAcc(year + 1, accDays + yearDays(year))
    val year = yearsGuessAcc(yearGuess, (Year1900 until yearGuess).map(yearDays).sum)

    val daysWithinYear = number - (Year1900 until year).map(yearDays).sum
    assert(daysWithinYear > 0, s"Number of days within guessed year $year is less than 1")

    def monthGuessAcc(month: Month.Value, accDays: Int): Month.Value =
      if (accDays + monthDays(year, month) >= daysWithinYear) month
      else monthGuessAcc(Month(month.id + 1), accDays + monthDays(year, month))
    val month = monthGuessAcc(Month.Jan, 0)
    val day = (number - dayNumber(year, month, 1) + 1).toInt

    assert(isValidDay(year, month, day), s"Calculated day of $day/$month/$year is invalid")
    (year, month, day)
  }

  def isValidDay(year: Int, month: Month.Value, day: Int): Boolean = day >= 1 && day <= monthDays(year, month)

  def monthDays(year: Int, month: Month.Value): Int = month match {
    case d31 if List(Month.Jan, Month.Mar, Month.May, Month.Jul, Month.Aug, Month.Oct, Month.Dec) contains d31 => 31
    case d30 if List(Month.Apr, Month.Jun, Month.Sep, Month.Nov) contains d30 => 30
    case feb => if ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0) 29 else 28
  }

  def yearDays(year: Int): Int = if (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)) 366 else 365

  def weekday(year: Int, month: Month.Value, day: Int): Weekday.Value = weekday(dayNumber(year, month, day))
  def weekday(number: Long): Weekday.Value = Weekday(((number - 1) % 7).toInt)

  /**
    * Returns list of days between inclusive start and end dates that match specified filter
    */
  def matchingDays(dayFilter: (Int, Month.Value, Int) => Boolean,
                  yearStart: Int, monthStart: Month.Value, dayStart: Int,
                  yearEnd: Int, monthEnd: Month.Value, dayEnd: Int): List[(Int, Month.Value, Int)] =
    matchingDays(dayFilter, dayNumber(yearStart, monthStart, dayStart), dayNumber(yearEnd, monthEnd, dayEnd)).map(day)
  def matchingDays(dayFilter: (Int, Month.Value, Int) => Boolean, start: Long, end: Long): List[Long] =
    (start to end).filter((n) => {
                                val (y, m, d)= day(n)
                                dayFilter(y, m, d)
                              }).toList
}

object Weekday extends Enumeration {
  val Mon = Value(0, "Mon")
  val Tue = Value(1, "Tue")
  val Wed = Value(2, "Wed")
  val Thu = Value(3, "Thu")
  val Fri = Value(4, "Fri")
  val Sat = Value(5, "Sat")
  val Sun = Value(6, "Sun")
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
