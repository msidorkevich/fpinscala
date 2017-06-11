package fpinscala.state

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary

class RNGSProperties extends Properties("RNG") {

  val genRNG: Gen[RNG] = for {
    seed <- arbitrary[Int]
  } yield RNG.Simple(seed)

  implicit val arbRNG = Arbitrary(genRNG)

  property("nonNegativeInt") = forAll { (rng: RNG) =>
    RNG.nonNegativeInt(rng)._1 >= 0
  }
}
