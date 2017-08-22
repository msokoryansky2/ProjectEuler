package com.msokoryansky.MathUtils

object Logical {
  def xor(c: Char, key: Char): Char = (c ^ key).toChar

  def xor(str: String, key: String): String = {
    require(key.nonEmpty, "Key cannot be empty")
    str.length match {
      case 0 => str
      case _ =>
        val key2 = key * (str.length / key.length) + key.substring(0, str.length % key.length)
        str.toList.zip(key2.toList).map(z => xor(z._1, z._2)).mkString
    }
  }
}
