package fpinscala.datastructures

import org.scalatest.FlatSpec

/**
 * Created by brianyarr on 03/03/15.
 */
class TestTree extends FlatSpec {

  val leafA = Leaf(1)
  val leafB = Leaf(4)
  val leafC = Leaf(9)
  val leafD = Leaf(2)
  val tree1 = Branch(Branch(leafA, leafB), Branch(leafC, leafD))

  it should "have size 7" in {
    assert(Tree.size(tree1) == 7)
  }

  it should "have max 9" in {
    assert(Tree.max(tree1) == 9)
  }

  it should "have depth 4" in {
    assert(Tree.depth(tree1) == 4)
  }

  it should "map each node " in {
    val t2 = Tree.map(tree1)((x) => 2*x)
    val exp = Branch(Branch(Leaf(2), Leaf(8)), Branch(Leaf(18), Leaf(4)))
    assert( t2 == exp )
  }
}
