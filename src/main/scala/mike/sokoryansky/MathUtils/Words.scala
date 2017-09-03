package mike.sokoryansky.MathUtils

import scala.collection.immutable.HashSet
import scala.io.Source

object Words {

  /**
    * Default indexed (for lookup) set of known words
    */
  def knownWords: HashSet[String] =
    new HashSet() ++ Source.fromResource("general_common_english_words.txt").getLines.map(_.toLowerCase)

  /**
    * Returns those of specified words that are found in common English words reference list. Case-insensitive
    */
  def words(words: Seq[String], knownWords: HashSet[String]): Seq[String] = {
    def knownWords2 = if (knownWords.isEmpty) Words.knownWords else knownWords
    words.filter(w => knownWords2.contains(w.toLowerCase))
  }
  def words(words: String, knownWords: HashSet[String]): Seq[String] = Words.words(words.split("[^\\w]+"), knownWords)
}
