package com.exercises.chapter2

import org.scalatest._

class SorterTest extends FlatSpec with Matchers {
  behavior of "Sorter"

  it should "test isSorted for Int array" in {
    Sorter.isSorted(Array(1, 2, 4, 5), (a: Int  , b: Int) => a <= b) should be (true)
    Sorter.isSorted(Array(1, 1, 1, 1), (a: Int, b: Int) => a <= b) should be (true)
    Sorter.isSorted(Array(1, 2, 7, 5), (a: Int, b: Int) => a <= b) should be (false)
  }

  it should "test isSorted for Char array" in {
    Sorter.isSorted(Array('a', 'c', 'd', 'h'), (a: Char  , b: Char) => a <= b) should be (true)
    Sorter.isSorted(Array('x', 'x'), (a: Char, b: Char) => a <= b) should be (true)
    Sorter.isSorted(Array('r', 'f', 'a'), (a: Char, b: Char) => a <= b) should be (false)
  }
}
