package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestCalendar extends FunSuite {
  test("monthDays returns number of days in specified month and year") {
    assert(Calendar.monthDays(1990, Month.Jan) === 31)
    assert(Calendar.monthDays(1990, Month.Feb) === 28)
    assert(Calendar.monthDays(2000, Month.Feb) === 29)
    assert(Calendar.monthDays(1900, Month.Feb) === 28)
    assert(Calendar.monthDays(1990, Month.Aug) === 31)
    assert(Calendar.monthDays(1990, Month.Dec) === 31)
  }

  test("yearDays returns number of days in specified year") {
    assert(Calendar.yearDays(1990) === 365)
    assert(Calendar.yearDays(1996) === 366)
    assert(Calendar.yearDays(2000) === 366)
    assert(Calendar.yearDays(1900) === 365)
  }

  test("dayNumber returns number of specified data relative to day #1 which is 1 Jan 1900") {
    intercept[Exception] {
      Calendar.dayNumber(1899, Month.Jan, 1)
    }
    intercept[Exception] {
      Calendar.dayNumber(1900, Month.Jan, 0)
    }
    intercept[Exception] {
      Calendar.dayNumber(1900, Month.Jan, 32)
    }
    intercept[Exception] {
      Calendar.dayNumber(1900, Month.Feb, 29)
    }
    assert(Calendar.dayNumber(1900, Month.Jan, 1) === 1)
    assert(Calendar.dayNumber(1900, Month.Jan, 31) === 31)
    assert(Calendar.dayNumber(1900, Month.Feb, 28) === 59)
    assert(Calendar.dayNumber(1900, Month.Mar, 1) === 60)
    assert(Calendar.dayNumber(1901, Month.Jan, 1) === 366)
    assert(Calendar.dayNumber(1901, Month.Mar, 5) === 429)
  }

  test("weekday returns day of the week") {
    assert(Calendar.weekday(1) === Weekday.Mon)
    assert(Calendar.weekday(2017, Month.Aug, 6) === Weekday.Sun)
  }

  test("day return year/month/day of specified day number") {
    assert(Calendar.day(1) === (1900, Month.Jan, 1))
    assert(Calendar.day(2) === (1900, Month.Jan, 2))
    assert(Calendar.day(20) === (1900, Month.Jan, 20))
    assert(Calendar.day(30) === (1900, Month.Jan, 30))
    assert(Calendar.day(31) === (1900, Month.Jan, 31))
    assert(Calendar.day(32) === (1900, Month.Feb, 1))
    assert(Calendar.day(59) === (1900, Month.Feb, 28))
    assert(Calendar.day(60) === (1900, Month.Mar, 1))
    assert(Calendar.day(65) === (1900, Month.Mar, 6))
    assert(Calendar.day(364) === (1900, Month.Dec, 30))
    assert(Calendar.day(365) === (1900, Month.Dec, 31))
    assert(Calendar.day(366) === (1901, Month.Jan, 1))
    assert(Calendar.day(429) === (1901, Month.Mar, 5))
    (1 to 100000 by 1000).foreach(d => assert(Calendar.dayNumber(Calendar.day(d)) == d))
  }

  test("matchingDays return list of threeples in specified range matching specified filter") {
    assert(Calendar.matchingDays((y, m, d) => Calendar.weekday(y, m, d) == Weekday.Mon,
      1900, Month.Jan, 1, 1900, Month.Jan, 31) ===
      List((1900, Month.Jan, 1), (1900, Month.Jan, 8), (1900, Month.Jan, 15),
        (1900, Month.Jan, 22), (1900, Month.Jan, 29)))
    assert(Calendar.matchingDays((y, m, d) => y % 25 == 0 && m == Month.Jan && d == 5,
      1901, Month.Jan, 1, 2015, Month.Jan, 31) ===
      List((1925, Month.Jan, 5), (1950, Month.Jan, 5), (1975, Month.Jan, 5), (2000, Month.Jan, 5)))
  }
}
