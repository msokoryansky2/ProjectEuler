package mike.sokoryansky.MathUtils

import IntegerOps._

object LatticePath {
  /**
    * Number of paths along the edges of a width x height lattice with starting point in upper left and every move
    * being down or to the right
    *
    * This is a correct but a naive/slow impl. See fast one below
    *
    * @param width lattice width. Number of vertical edges is, by definition, width + 1
    * @param height lattice height. Number of horizontal edges is, by definition, height + 1
    * @return number of possible paths
    */
  def numPathsSlow(width: Int, height: Int): Long = {
    require(width >= 0 && height >= 0 && !(width == 0 && height == 0), "Lattice must have positive width and height")
    if (width == 0 || height == 0) 1
    else (width, height) match {
      case (x, y) if x > 1 && y > 1 => if (x == y) 2 * numPaths(x - 1, y) else numPaths(x - 1, y) + numPaths(x, y - 1)
      case (x, y) => Math.max(x, y) + 1
    }
  }

  /**
    * Same as above but optimal impl that realizes that this is an instanace of n-choose-k problem.
    */
  def numPaths(width: Int, height: Int): Long = {
    require(width >= 0 && height >= 0 && !(width == 0 && height == 0), "Lattice must have positive width and height")
    ((BigInt(width) + BigInt(height)).factorial / (BigInt(width).factorial * BigInt(height).factorial)).toLong
  }
}
