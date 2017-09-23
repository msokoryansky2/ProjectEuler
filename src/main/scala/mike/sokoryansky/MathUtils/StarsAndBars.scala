package mike.sokoryansky.MathUtils

/**
  * Exploring how integers can be composed as a sum of ALL smaller integers.
  *
  * When thinking about these, we view an integer N as N stars and its parts as divider bars among those stars.
  * E.g.
  *
  * 7 == * * * * * * *
  * 7 as (3 + 4) == * * * | * * * *
  * 7 as (4 + 3) == * * * * | * * *
  *
  * Note that in some instances we may consider sums (3 + 4) and (4 + 3) to be the same, and in others, different.
  * In cases where (3 + 4) is different from (4 + 3), the total number of sums for any given number N using K  bars
  * (K < N) is number of places those K bars can be placed into N - 1 possible slots between N stars. In other words,
  * "(N - 1) choose K" which is: (N - 1)! / ((K!) * (N - 1 - K)!)
  *
  * See https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics) for insights and reason for Stars-and-Bars name
  */
object StarsAndBars {
  /**
    * Count number of ways specific number of bars can be placed for specific number of stars.
    * Reduces to simple (stars - 1) choose (bars)
    */
  def count(stars: Long, bars: Long): Long = {
    import IntegerOps._
    // Spell out some trivial cases which may or may not speed things up
    if (bars == 0 || bars == (stars - 1)) 1
    else if (bars == 1) stars - 1
    else (stars - 1).factorial / (bars.factorial * (stars - 1 - bars).factorial)
  }

  /**
    * Count number of ways all posible bars from 0 (if zeroBar == true) or 1 to (stars - 1) can be placed between stars
    */
  def countAllBars(stars: Long, zeroBar: Boolean = true): Long =
    ((if (zeroBar) 0 else 1) to stars - 1).map(bars => count(stars, bars)).sum

  /**
    * Create a map for all possible tuples of (stars from 1 to starsMax  and (bars from 0 (if zeroBar == true) or 1 to
    * (star - 1) for each star) to the number of *distinct* ways those bars can be placed within those stars.
    *
    * In this context distinct refers to the fact that "* * * | * * * *" is the same as "* * * * | * * *"  because
    * we are thinking in terms of sums of differents sets of integers and Set(3, 4) is the same as Set(4, 3)
    */
  def countDistinctAllStarsAllBars(starsMax: Long, zeroBar: Boolean = true): Map[(Long, Long), Long] = {
    val barsMin = if (zeroBar) 0 else 1
    require(starsMax >= barsMin + 1, "Max number of stars must be > min number of bars")
    def countAcc(stars: Long, bars: Long, acc: Map[(Long, Long), Long]): Map[(Long, Long), Long] = {
      if (stars > starsMax) acc
      else if (bars > stars - 1) countAcc(stars + 1, barsMin, acc)
      else
        countAcc(stars,
                  bars + 1, 
                  acc ++ Map((stars, bars) -> (1 to (stars - bars)).map(b => acc(stars - b, bars - 1)).sum))
    }
    if (zeroBar) countAcc(2, barsMin, Map((1L, 0L) -> 1L)) else countAcc(3, barsMin, Map((2L, 1L) -> 1L))
  }
}