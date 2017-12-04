package mike.sokoryansky.MathUtils

import org.scalatest.FunSuite

class TestNumberField extends FunSuite {
  test("Non-rectangular or empty fields are invalid") {
    intercept[Exception] {
      NumberField(
        """
          |123  145
          |1236 63662 35
          |56   232
        """.stripMargin)
    }

    intercept[Exception] {
      NumberField(
        """
          |
        """.stripMargin)
    }
  }

  test("Rectangular field are cool") {
    val field = NumberField(
      """
        |123 , 0145
        |1236 ,63662
        |56,0232
      """.stripMargin)

    assert(field.height === 3)
    assert(field.width === 2)
    assert(field.elSize === 5)
    assert(field.el(0, 0) === 123)
    assert(field.el(1, 2) === 232)
  }

  test("allPathsBruteForce enumerates all possible paths") {
    val field = NumberField(
      """
        |123  0145
        |56   0232
      """.stripMargin)

    val allPaths = field.allPathsBruteForce.toSet
    assert(allPaths === Set(
      List((0, 0), (0, 1), (1, 1)),
      List((0, 0), (1, 0), (1, 1))))

    val field2 = NumberField(
      """
        |123  0145 355
        |1236 63662 -36
        |56   0232 35
      """.stripMargin)

    val allPaths2 = field2.allPathsBruteForce.toSet
    assert(allPaths2 === Set(
      List((0, 0), (0, 1), (0, 2), (1, 2), (2, 2)),
      List((0, 0), (0, 1), (1, 1), (1, 2), (2, 2)),
      List((0, 0), (0, 1), (1, 1), (2, 1), (2, 2)),
      List((0, 0), (1, 0), (1, 1), (1, 2), (2, 2)),
      List((0, 0), (1, 0), (1, 1), (2, 1), (2, 2)),
      List((0, 0), (1, 0), (2, 0), (2, 1), (2, 2))))
  }

  private val field = NumberField(
    """
      |001 002 003 004
      |005 006 007 009
      |010 011 012 013
      |001 002 003 004
    """.stripMargin)
  private val path = field.bestPath()
  private val pathMin = field.bestPathMin()
  private val pathSlow = field.bestPathSlow()
  private val pathBrute = field.bestPathBruteForce

  private val field2 = NumberField(
    """
      |001, 002, 3, 04
      |5, 006, 007, 009
      |010, -11, 12, -013
      |001, 02, 003, 0004
    """.stripMargin)
  private val path2: List[(Int, Int)] = field2.bestPath()
  private val pathSlow2 = field2.bestPathSlow()
  private val pathBrute2 = field2.bestPathBruteForce

  private val field3 = NumberField(
    """
      |001 002 003 004
      |005 006 007 019
      |-10 -11 -12 -13
      |001 002 003 004
    """.stripMargin)
  private val path3: List[(Int, Int)] = field3.bestPath()
  private val pathSlow3 = field3.bestPathSlow()
  private val pathBrute3 = field3.bestPathBruteForce

  test("toString creates a visual representation of the field") {
    assert(field3.toString ===
      """|  1   2   3   4
         |  5   6   7  19
         |-10 -11 -12 -13
         |  1   2   3   4""".stripMargin)
  }

  test("bestPath returns path resulting in largest sum of visited numbers") {
    assert(path === List((0, 0), (0, 1), (0, 2), (1, 2), (2, 2), (3, 2), (3, 3)))
    assert(path2 === List((0, 0), (0, 1), (1, 1), (2, 1), (2, 2), (2, 3), (3, 3)))
    assert(path3 === List((0, 0), (0, 1), (1, 1), (2, 1), (3, 1), (3, 2), (3, 3)))
  }

  test("bestPathMin returns path resulting in smallest sum of visited numbers") {
    assert(pathMin === List((0, 0), (0, 1), (0, 2), (0, 3), (1, 3), (2, 3), (3, 3)))
  }

  test("bestPathSlow should be same as bestPath") {
    assert(path === pathSlow)
    assert(path2 === pathSlow2)
    assert(path3 === pathSlow3)
  }

  test("bestPathBruteForce should be same as bestPath") {
    assert(path === pathBrute)
    assert(path2 === pathBrute2)
    assert(path3 === pathBrute3)
  }

  test("evalPath evaluates path value") {
    assert(field.evalPath(path) === 56)
    assert(field2.evalPath(path2) === 38)
    assert(field3.evalPath(path3) === 29)

    assert(field.evalPath(path) === 56)
    assert(field2.evalPath(path2) === 38)
    assert(field3.evalPath(path3) === 29)
  }

  test("pathToString creates a visual representation of a path") {
    assert(field.pathToString(path) ===
      """| 1 .. .. ..
         | 5 .. .. ..
         |10 11 12 13
         |.. .. ..  4""".stripMargin)

    assert(field2.pathToString(path2) ===
      """|  1 ... ... ...
         |  5   6   7 ...
         |... ...  12 ...
         |... ...   3   4""".stripMargin)

    assert(field3.pathToString(path3) ===
      """|  1 ... ... ...
         |  5   6   7  19
         |... ... ... -13
         |... ... ...   4""".stripMargin)
  }

  test("NumberField can be randomly generated too") {
    val field = NumberField(40, 40, (5 to 8).toSet)
    val path = field.bestPath()
    assert(path.size === 79)
    assert(field.evalPath(path) >= 395)

    val field2 = NumberField(40, 25, (10 to 20).toSet)
    val path2 = field2.bestPath()
    assert(path2.size === 64)
    assert(field2.evalPath(path2) >= 640)

    val field3 = NumberField(25, 40, (10 to 20).toSet)
    val path3 = field3.bestPath()
    assert(path3.size === 64)
    assert(field3.evalPath(path3) >= 640)
  }
}
