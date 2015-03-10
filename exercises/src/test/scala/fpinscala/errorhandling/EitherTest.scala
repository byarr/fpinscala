package fpinscala.errorhandling

import org.scalatest.FlatSpec

class EitherTest extends FlatSpec{

  val leftEither: Either[String, Int] = Left("Error")
  val rightEither: Either[String, Int] = Right(5)

  "left either" should " map to left " in {
    assert(leftEither.map((x) => x *2) == leftEither)
  }

  "right either" should " map to f(right) " in {
    assert(rightEither.map((x) => x *2) == Right(10))
  }

  "left orElse" should "return else" in {
    assert(leftEither.orElse(rightEither) == rightEither)
  }

}
