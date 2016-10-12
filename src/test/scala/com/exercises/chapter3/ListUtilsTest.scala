package com.exercises.chapter3

import org.scalatest._

class ListUtilsTest extends FlatSpec with Matchers {
  "ListUtils" should "return tail list" in {
    ListUtils.tail(List(1, 2, 3, 4, 5)) should be(List(2, 3, 4, 5))
  }

  "ListUtils" should "drop first n elements from list" in {
    ListUtils.drop(List(1, 2, 3, 4, 5), 0) should be(List(1, 2, 3, 4, 5))
    ListUtils.drop(List(1, 2, 3, 4, 5), 2) should be(List(3, 4, 5))
  }

  "ListUtils" should "dropUntil condition is true from list" in {
    ListUtils.dropUntil(List(1, 2, 3, 4, 5))(x => x < 3) should be(List(3, 4, 5))
    ListUtils.dropUntil(List(1, 2, 3, 4, 5))(x => x < 10) should be(List.empty)
  }

  "ListUtils" should "setHead to given list" in {
    ListUtils.setHead(List(1, 2, 3, 4, 5), 10) should be(List(10, 2, 3, 4, 5))
    ListUtils.setHead(Nil, 1) should be(List(1))
  }

  "ListUtils" should "return initial elements of list excluding last element" in {
    ListUtils.init(List(1, 2, 3, 4, 5)) should be(List(1, 2, 3, 4))
    ListUtils.init(List(1)) should be(Nil)
    ListUtils.init(Nil) should be(Nil)
  }

  "ListUtils" should "calculate product of list with fold right" in {
    ListUtils.product(List(1, 2, 3)) should be(6)
  }

  "ListUtils" should "calculate length of list with fold right" in {
    ListUtils.length(List(1, 2, 3)) should be(3)
    ListUtils.length(Nil) should be(0)
    ListUtils.length(List.empty) should be(0)
  }

  "ListUtils" should "implement foldLeft using tailRecursion" in {
    ListUtils.foldLeft(List(1, 2, 3), 1)((a, b) => a * b) should be(6)
    ListUtils.foldLeft(List.empty[Int], 1)((a, b) => a * b) should be(1)
  }

  "ListUtils" should "implement reverse using foldLeft" in {
    ListUtils.reverse(List(1, 2, 3)) should be(List(3, 2, 1))
    ListUtils.reverse(List.empty) should be(List.empty)
  }

  "ListUtils" should "implement append using foldLeft" in {
    ListUtils.append(List(1, 2, 3), 4) should be(List(1, 2, 3, 4))
    ListUtils.append(List.empty, 4) should be(List(4))
  }

  "ListUtils" should "implement foldLeft using foldRight" in {
    ListUtils.foldLeft(List(1, 2, 3), 1)((a, b) => a * b) should be(6)
    ListUtils.foldLeft(List.empty[Int], 1)((a, b) => a * b) should be(1)
  }

  "ListUtils" should "implement append two lists using foldLeft" in {
    ListUtils.append(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
    ListUtils.append(List.empty, List(4)) should be(List(4))
    ListUtils.append(List(4), List.empty) should be(List(4))
    ListUtils.append(List.empty, List.empty) should be(List.empty)
  }

  "ListUtils" should "implement map" in {
    ListUtils.map(List(1, 2, 3))(x => x + 1) should be(List(2, 3, 4))
    ListUtils.map(List.empty[Int])(x => x + 1) should be(List.empty)
  }

  "ListUtils" should "implement filter" in {
    ListUtils.filter(List(1, 2, 3, 4, 5, 6))(x => x % 2 == 0) should be(List(1, 3, 5))
    ListUtils.filter(List(1, 2, 3, 4, 5, 6))(_ => true) should be(List.empty)
    ListUtils.filter(List.empty[Int])(x => x / 2 == 1) should be(List.empty)
  }

  "ListUtils" should "implement flatMap" in {
    ListUtils.flatMap(List("abcd", "xyz"))(x => x.toCharArray.toList) should be(List('a', 'b', 'c', 'd', 'x', 'y', 'z'))
  }

  "ListUtils" should "implement filter using flatMap" in {
    ListUtils.filter1(List(1, 2, 3, 4, 5, 6))(x => x % 2 == 0) should be(List(1, 3, 5))
    ListUtils.filter1(List(1, 2, 3, 4, 5, 6))(_ => true) should be(List.empty)
    ListUtils.filter1(List.empty[Int])(x => x / 2 == 1) should be(List.empty)
  }

  "ListUtils" should "add two lists" in {
    ListUtils.add(List(1, 2, 3), List(5, 6, 7)) should be(List(6, 8, 10))
    ListUtils.add(List.empty, List.empty) should be(List.empty)

    intercept[IllegalArgumentException](ListUtils.add(List(1, 2), List(1)))
  }

  "ListUtils" should "merge two lists with given function applied" in {
    ListUtils.mergeWith(List(1, 2, 3), List(5, 6, 7))((a, b) => a + b) should be(List(6, 8, 10))
    ListUtils.mergeWith(List.empty[Int], List.empty[Int])((a, b) => a + b) should be(List.empty)

    intercept[IllegalArgumentException](ListUtils.mergeWith(List(1, 2), List(1))((a, b) => a + b))
  }

  "ListUtils" should "take first n elements from given list" in {
    ListUtils.take(List(1, 2, 3), 2) should be(List(1, 2))
    ListUtils.take(List(1, 2, 3), 0) should be(List.empty)
    ListUtils.take(List(1, 2, 3), 10) should be(List(1, 2, 3))
  }

  "ListUtils" should "implement slide function on list" in {
    ListUtils.slide(List(1, 2), 3) should be(List(List(1, 2)))
    ListUtils.slide(List(1, 2, 3), 2) should be(List(List(1, 2), List(2, 3)))
  }

  "ListUtils" should "check whether a list is subSequence of other" in {
    ListUtils.hasSubsequence(List(1, 2), List(1,2)) should be(true)
    ListUtils.hasSubsequence(List(1, 2), List(1)) should be(true)
    ListUtils.hasSubsequence(List(1, 2), List(2)) should be(true)

    ListUtils.hasSubsequence(List(1, 2), List(1,2,3)) should be(false)
    ListUtils.hasSubsequence(List.empty, List(1)) should be(false)
    ListUtils.hasSubsequence(List(1), List.empty) should be(false)
  }
}
