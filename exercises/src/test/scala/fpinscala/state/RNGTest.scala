package fpinscala.state

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class RNGTest extends FunSuite with BeforeAndAfter{

  private var initialRNG = _

  before {
    initialRNG = RNG.Simple(1)
  }

  test("nonNegativeInt") {
    (-1000 to 1000).foreach(i => {
      val initialRNG = RNG.Simple(i)
      val (nonNegative, _) = RNG.nonNegativeInt(initialRNG)
      if (nonNegative < 0) println(i)
      assert(nonNegative >= 0)
    })
  }

  test("nonNegativeInt corner case") {
    val (nonNegative, _) = RNG.nonNegativeInt(initialRNG)
    assert(nonNegative >= 0)
  }

  test("double") {
    val (double, _) = RNG.double(initialRNG)
    assert(double.isWhole() === false)
  }

  test("intDouble") {
    val ((int, double), _) = RNG.intDouble(initialRNG)

    assert(int.isWhole() === true)
    assert(double.isWhole() === false)
  }

  test("doubleInt") {
    val ((double, int), _) = RNG.doubleInt(initialRNG)

    assert(int.isWhole() === true)
    assert(double.isWhole() === false)
  }

  test("double3") {
    val ((d1, d2, d3), _) = RNG.double3(initialRNG)

    assert(d1.isWhole() === false)
    assert(d2.isWhole() === false)
    assert(d3.isWhole() === false)
  }
}
