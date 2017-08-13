package com.msokoryansky.MathUtils

object String {
  
}

object StringOps {
  implicit class StringUtilityOps(s: String) {
    def rotate(n: Int): String = {
      n match {
        case 0 => s
        case pos if pos > 0 => s.substring(s.length - (n % s.length)) + s.substring(0, s.length - (n % s.length))
        case neg if neg < 0 => rotate(s.length - (Math.abs(n) % s.length))
      }
    }
  }
}