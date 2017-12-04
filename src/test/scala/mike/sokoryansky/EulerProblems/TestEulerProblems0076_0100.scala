package mike.sokoryansky.EulerProblems

import org.scalatest.FunSuite

class TestEulerProblems0076_0100 extends FunSuite {
  test("#76") {
    assert((new P0076).run === "190569291")
  }

  test("#77") {
    assert((new P0077).run === "71")
  }

  test("#78") {
    assert((new P0078).run === "55374")
  }

  test("#79") {
    assert((new P0079).run === "73162890")
  }

  test("#80") {
    assert((new P0080).run === "40886")
  }
}
