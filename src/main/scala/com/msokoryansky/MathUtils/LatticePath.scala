package com.msokoryansky.MathUtils

object LatticePath {
  /**
    * Number of paths along the edges of a width x height lattice with starting point in upper left and every move
    * being down or to the right
    *
    * TODO: Can probably be optimized quite a bit by keeping a history of already calculated numPaths 
    *
    * @param width lattice width. Number of vertical edges is, by definition, width + 1
    * @param height lattice height. Number of horizontal edges is, by definition, height + 1
    * @return number of possible paths
    */
  def numPaths (width: Int, height: Int): Long = (width, height) match {
      case (x, y) if x > 1 && y > 1 => if (x == y) 2 * numPaths(x - 1, y) else numPaths(x - 1, y) + numPaths(x, y - 1)
      case (x, y) => Math.max(x, y) + 1
  }
}
