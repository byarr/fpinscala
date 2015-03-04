package fpinscala.errorhandling

import org.scalatest.FlatSpec

/**
 * Created by brianyarr on 04/03/15.
 */
class OptionTest extends FlatSpec{

  "Some" should "map to some" in {
    val optInt = Some(3)
    assert(optInt.map(x => 2 * x) == Some(6))
  }

  "None" should "map should to None" in {
    val optInt : Option[Int] = None
    assert(optInt.map(x => 2 * x) == None)
  }

  "Some" should "getOrElse to val" in {
    val optInt = Some(3)
    assert(optInt.getOrElse(22) == 3)
  }

  "None" should "getOrElse to param" in {
    val optInt : Option[Int] = None
    assert(optInt.getOrElse(22) == 22)
  }

  "Some" should "flatmap to some when f maps to Some" in {
    val optInt = Some(3)
    assert(optInt.flatMap(x => Some(2 * x)) == Some(6))
    assert(optInt.flatMap2(x => Some(2 * x)) == Some(6))
  }

  "None" should "flatMap to None when f maps to Some" in {
    val optInt : Option[Int] = None
    assert(optInt.flatMap(x => Some(2 * x)) == None)
    assert(optInt.flatMap2(x => Some(2 * x)) == None)
  }

  "Some" should "flatmap to None when f maps to None" in {
    val optInt = Some(3)
    assert(optInt.flatMap(x => None) == None)
    assert(optInt.flatMap2(x => None) == None)
  }

  "None" should "flatMap to None when f maps to None" in {
    val optInt : Option[Int] = None
    assert(optInt.flatMap(x => None) == None)
    assert(optInt.flatMap2(x => None) == None)

  }


  "Some" should "orElse to val" in {
    val optInt = Some(3)
    assert(optInt.orElse2(Some(34)) == optInt)
    assert(optInt.orElse(Some(34)) == optInt)
  }

  "None" should "orElse to param" in {
    val optInt : Option[Int] = None
    assert(optInt.orElse2(Some(34)) == Some(34))
    assert(optInt.orElse(Some(34)) == Some(34))
  }


}
