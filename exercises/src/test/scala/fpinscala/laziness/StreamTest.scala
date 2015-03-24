package fpinscala.laziness

import org.scalatest.FlatSpec

/**
 * Created by brianyarr on 24/03/15.
 */
class StreamTest extends FlatSpec {

  val stream1to3: Stream[Int] = Stream.cons(  1, Stream.cons( 2, Stream.cons( 3, Stream.empty)))

  it should "convert toList" in {
    assert(stream1to3.toList == List(1,2,3))
  }

  it should "find when exists" in {
    assert(stream1to3.exists(p => p == 2))
  }

  it should "not find when not exists" in {
    assert(!stream1to3.exists(p => p == 4))
  }

  it should "take" in {
    val stream1to2: Stream[Int] = Stream.cons(1, Stream.cons(2, Stream.empty))
    assert(stream1to3.take(2).toList == stream1to2.toList)
  }

  it should "drop" in {
    val stream2to2: Stream[Int] = Stream.cons(2, Stream.cons(3, Stream.empty))
    assert(stream1to3.drop(1).toList == stream2to2.toList)
  }

  it should "takeWhile" in {
    val stream1to2: Stream[Int] = Stream.cons(1, Stream.cons(2, Stream.empty))
    assert(stream1to3.takeWhile(i => i < 3).toList == stream1to2.toList)
  }

}
