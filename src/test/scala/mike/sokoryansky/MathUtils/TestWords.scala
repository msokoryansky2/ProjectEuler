package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

import scala.collection.immutable.HashSet

class TestWords extends FunSuite {
  val knownWords: HashSet[String] = Words.knownWords

  test("knownWords contains common English words in lower case") {
    assert(knownWords.contains("hello"))
    assert(knownWords.contains("aardvark"))
    assert(!knownWords.contains("rywerydfh34gsd"))
  }

  test("count counts how many of specified words are known to be English words") {
    assert(Words.words(List("fasywetw", "a", "ThE", "aarDVark", "asdgasdgs", "lose", "compute"), HashSet[String]()) ===
      List("a", "ThE", "aarDVark", "lose", "compute"))
    assert(Words.words("fasywetw, a, ThE, aarDVark, asdgasdgs lose . compute", HashSet[String]()) ===
      List("a", "ThE", "aarDVark", "lose", "compute"))

    assert(Words.words(List("fasywetw", "a", "ThE", "aarDVark", "asdgasdgs", "lose", "compute"), knownWords) ===
      List("a", "ThE", "aarDVark", "lose", "compute"))
    assert(Words.words("fasywetw, a, ThE, aarDVark, asdgasdgs lose . compute", knownWords) ===
      List("a", "ThE", "aarDVark", "lose", "compute"))

    assert(Words.words(List("fasywetw", "a", "ThE", "aarDVark", "asdgasdgs", "lose", "compute"), HashSet("the", "a")) ===
      List("a", "ThE"))
    assert(Words.words("fasywetw, a, ThE, aarDVark, asdgasdgs lose . compute", HashSet("the", "a")) ===
      List("a", "ThE"))
  }
}
