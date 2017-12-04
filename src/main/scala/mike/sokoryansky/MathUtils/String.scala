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

  /**
    * Returns shortest string that has all of specified character sequences (chars in order but possibly not consecutive),
    * where char sequences are specified as Strings.
    *
    * The general algo is to discover the first char which only appears in first place in all seqs. That char
    * is the first char in the result String. Then remove the first occurrence of that char from all seqs and repeat.
    * Keep repeating until no seqs are left.
    */
  def shortestWithScatterSequences(seqs: List[String]): String = {
    def shortestWithScatterSequencesAcc(seqs: List[String], acc: String): String = {
      // Get all unique chars appearing in seqs
      val chars: List[Char] = seqs.mkString.toCharArray.distinct.toList
      if (chars.isEmpty) acc
      else {
        // We try to find an undisputed first char (that only appears in first position)
        // and fall back on randomly selected first of the ones that appear in first position
        // for seqs like ("ABC", "BCA", "CAB").
        val c = chars.find(c => !seqs.exists(s => s.contains(c) && s(0) != c))
          .getOrElse(chars.find(c => seqs.exists(s => s(0) == c)).get)
        shortestWithScatterSequencesAcc(seqs.map(s => if (s.length > 0 && s(0) == c) s.substring(1) else s), acc + c)
      }
    }
    shortestWithScatterSequencesAcc(seqs, "")
  }
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