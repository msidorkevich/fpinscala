package fpinscala.state

import org.scalatest.FunSuite

class RNGTest extends FunSuite {
  test("nonNegativeInt") {
    (-1000 to 1000).foreach(i => {
      val initialRNG = RNG.Simple(i)
      val (nonNegative: Int, newRNG: RNG) = RNG.nonNegativeInt(initialRNG)
      if (nonNegative < 0) println(i)
      assert(nonNegative >= 0)
    })
  }

  test("nonNegativeInt corner case") {
    val initialRNG = RNG.Simple(-2071084361)
    val (nonNegative: Int, newRNG: RNG) = RNG.nonNegativeInt(initialRNG)
    assert(nonNegative >= 0)
  }
}
