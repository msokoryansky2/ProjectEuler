package com.msokoryansky.MathUtils

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
    assert(Calendar.day(429) === (1901, Month.Mar, 5))
    (1 to 100000 by 1000).foreach(d => assert(Calendar.dayNumber(Calendar.day(d)) == d))
  }
}
