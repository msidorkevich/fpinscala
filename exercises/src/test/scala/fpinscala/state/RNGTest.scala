package fpinscala.state

import fpinscala.state.RNG.Rand
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class RNGTest extends FunSuite with BeforeAndAfter {

  private var initialRNG: RNG = _

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

  test("ints") {
    val count = 5
    val (intList, _) = RNG.ints(count)(initialRNG)

    assert(intList.size === count)
  }

  test("ints of 0 size") {
    val (intList, _) = RNG.ints(0)(initialRNG)

    assert(intList.isEmpty === true)
  }

  test("map2") {
    val ra: Rand[Int] = RNG.unit(12345)
    val rb: Rand[Int] = RNG.unit(23456)
    val randSum: Rand[Int] = RNG.map2(ra, rb)(_ + _)

    val (sum, _) = randSum.apply(initialRNG)

    assert(sum === 12345 + 23456)
  }

  test("sequence") {
    val ra: Rand[Int] = RNG.unit(12345)
    val rb: Rand[Int] = RNG.unit(23456)
    val rc: Rand[Int] = RNG.unit(34567)

    val seq: Rand[List[Int]] = RNG.sequence(List(ra, rb, rc))
    val list = seq.apply(initialRNG)._1

    assert(list === List(12345, 23456, 34567))
  }

  test("flatMap") {
    val ra: Rand[Int] = RNG.unit(12345)
    val raPlusOne: Rand[Int] = RNG.flatMap(ra)(a => RNG.unit(a + 1))

    val (raPlusOneValue, _) = raPlusOne.apply(initialRNG)

    assert(raPlusOneValue === 12345 + 1)
  }
}
