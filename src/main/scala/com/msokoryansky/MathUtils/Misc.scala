package com.msokoryansky.MathUtils

import scala.annotation.tailrec

object Misc {
  def dropFirstMatch[A](ls: Seq[A], value: A): Seq[A] = {
    val index = ls.indexOf(value)
    if (index < 0) ls
    else if (index == 0) ls.tail
    else {
      val (a, b) = ls.splitAt(index)
      a ++ b.tail
    }
  }

  def union2[A](a: Seq[A], b: Seq[A]): Seq[A] = {
    @tailrec def union2Acc(a: Seq[A], b: Seq[A], acc: Seq[A]): Seq[A] = {
      if (a.isEmpty && b.isEmpty) acc
      else if (a.isEmpty) union2Acc(b, Seq.empty, acc)
      else union2Acc(a.tail, dropFirstMatch(b, a.head), acc :+ a.head)
    }
    union2Acc(a, b, Seq.empty)
  }
}
