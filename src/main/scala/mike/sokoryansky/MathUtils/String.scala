package mike.sokoryansky.MathUtils

object String {
  /**
    * Returns 1 for a/A, 2, for b/B, 3 for c/C, etc
    */
  def letterValue(c: Char): Int = c.toUpper - 'A'.toUpper + 1

  /**
    * Returns value of a word by adding up values of its letters
    */
  def wordValue(s: String): Int = s.map(letterValue).sum
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