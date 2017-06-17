package fpinscala.state

import org.scalatest.FunSuite

class RNGTest extends FunSuite {
  test("nonNegativeInt") {
    (-1000 to 1000).foreach(i => {
      val initialRNG = RNG.Simple(i)
      val (nonNegative, _) = RNG.nonNegativeInt(initialRNG)
      if (nonNegative < 0) println(i)
      assert(nonNegative >= 0)
    })
  }

  test("nonNegativeInt corner case") {
    val initialRNG = RNG.Simple(-2071084361)
    val (nonNegative, _) = RNG.nonNegativeInt(initialRNG)
    assert(nonNegative >= 0)
  }

  test("double") {
    val initialRNG = RNG.Simple(1)
    val (double, _) = RNG.double(initialRNG)
    assert(double.isWhole() === false)
  }

  test("intDouble") {
    val initialRNG = RNG.Simple(1)
    val ((int, double), _) = RNG.intDouble(initialRNG)

    assert(int.isWhole() === true)
    assert(double.isWhole() === false)
  }

  test("doubleInt") {
    val initialRNG = RNG.Simple(1)
    val ((double, int), _) = RNG.doubleInt(initialRNG)

    assert(int.isWhole() === true)
    assert(double.isWhole() === false)
  }

  test("double3") {
    val initialRNG = RNG.Simple(1)
    val ((d1, d2, d3), _) = RNG.double3(initialRNG)

    assert(d1.isWhole() === false)
    assert(d2.isWhole() === false)
    assert(d3.isWhole() === false)
  }
}
