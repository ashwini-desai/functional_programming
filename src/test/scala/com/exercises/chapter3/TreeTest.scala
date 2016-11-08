package com.exercises.chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers{
  behavior of "Tree"

  it should "return size that counts the number of nodes of the given tree" in {
    Tree.size(Tree()) should be(0)
    Tree.size(Tree(1)) should be(1)
    Tree.size(Tree(Tree(1), Tree(Tree(1), Tree(1)))) should be(5)
  }

  it should "return max element of tree" in {
    Tree.max(Tree(1)) should be(1)
    Tree.max(Tree(Tree(Tree(3), Tree(2)), Tree(Tree(5), Tree(1)))) should be(5)

    intercept[UnsupportedOperationException](Tree.max(Tree()))
  }

  it should "return max depth of tree" in {
    Tree.depth(Tree(1)) should be(0)
    Tree.depth(Tree(Tree(1), Tree(Tree(1), Tree(1)))) should be(2)
    Tree.depth(Tree(Tree(Tree(3), Tree(2)), Tree(Tree(5), Tree(1)))) should be(2)

    intercept[UnsupportedOperationException](Tree.depth(Tree()))
  }

  it should "apply given function on every element of tree" in {
    Tree.map(Tree(1))((x: Int) => x+1) should be(Tree(2))
    Tree.map(Tree(Tree(1), Tree(Tree(2), Tree(3))))((x: Int) => x+1) should
      be(Tree(Tree(2), Tree(Tree(3), Tree(4))))

    Tree.map[Int, Int](Tree())((x: Int) => x+1) should be(Tree())
  }

  it should "implement size function using fold" in {
    Tree.size1(Tree()) should be(0)
    Tree.size1(Tree(1)) should be(1)
    Tree.size1(Tree(Tree(1), Tree(Tree(1), Tree(1)))) should be(5)
  }

  it should "implement max function using fold" in {
    Tree.max1(Tree()) should be(Int.MinValue)
    Tree.max1(Tree(1)) should be(1)
    Tree.max1(Tree(Tree(Tree(3), Tree(2)), Tree(Tree(5), Tree(1)))) should be(5)
  }

  it should "implement depth function using fold" in {
    Tree.depth1(Tree()) should be(0)
    Tree.depth1(Tree(1)) should be(0)
    Tree.depth1(Tree(Tree(1), Tree(Tree(1), Tree(1)))) should be(2)
    Tree.depth1(Tree(Tree(Tree(3), Tree(2)), Tree(Tree(5), Tree(1)))) should be(2)
  }

  it should "implement map function using fold" in {
    Tree.map1(Tree(1))((x: Int) => x+1) should be(Tree(2))
    Tree.map1(Tree(Tree(1), Tree(Tree(2), Tree(3))))((x: Int) => x+1) should
      be(Tree(Tree(2), Tree(Tree(3), Tree(4))))

    Tree.map1[Int, Int](Tree())((x: Int) => x+1) should be(Tree())
  }
}
