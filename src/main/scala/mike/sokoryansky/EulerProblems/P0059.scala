package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Logical
import mike.sokoryansky.MathUtils.Words

import scala.io.Source

class P0059 extends EulerProblem {
  def run: String = {
    val knownWords = Words.knownWords
    val cipher = Source.fromResource("p059_cipher.txt").getLines.mkString.split(",").map(_.toInt.toChar).mkString
    val key = (for {
                a <- 'a' to 'z'
                b <- 'a' to 'z'
                c <- 'a' to 'z'
                key = List(a, b, c).mkString
                decrypt = Logical.xor(cipher, key)
              } yield key -> Words.words(decrypt, knownWords).size).maxBy(_._2)._1
    val plaintext = Logical.xor(cipher, key)
    plaintext.toList.map(_.toInt).sum.toString
  }
}

object P0059 extends App {
  (new P0059).printAnswer()
}
