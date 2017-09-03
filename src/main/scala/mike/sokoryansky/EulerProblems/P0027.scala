package mike.sokoryansky.EulerProblems

import mike.sokoryansky.MathUtils.Prime
import scala.annotation.tailrec

/*
Euler discovered the remarkable quadratic formula:

n2+n+41

It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤39
. However, when n=40,402+40+41=40(40+1)+41 is divisible by 41, and certainly when n=41,412+41+41

is clearly divisible by 41.

The incredible formula n2−79n+1601
was discovered, which produces 80 primes for the consecutive values 0≤n≤79

. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n2+an+b

, where |a|<1000 and |b|≤1000

where |n|
is the modulus/absolute value of n
e.g. |11|=11 and |−4|=4

Find the product of the coefficients, a
and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n=0.
 */

class P0027 extends EulerProblem {
  def evalQuadratic(a: Int, b: Int, n: Int): Int = n * n + a * n + b

  def quadraticPrimes(a: Int, b: Int): List[Int] = {
    @tailrec def quadraticPrimesAcc(n: Int, acc: List[Int]): List[Int] = {
      val eval = evalQuadratic(a, b, n)
      if (!Prime.isPrime(eval)) acc else quadraticPrimesAcc(n + 1, eval :: acc)
    }
    quadraticPrimesAcc(0, List[Int]())
  }

  def run: String = {
    val tupleABNumQuadPrimes = (for {
      a <- -1000 to 1000
      b <- -1000 to 1000
      primesCount = quadraticPrimes(a, b).length
    } yield (a, b, primesCount)).maxBy(_._3)
    (tupleABNumQuadPrimes._1 * tupleABNumQuadPrimes._2).toString
  }
}

object P0027 extends App {
  (new P0027).printAnswer()
}