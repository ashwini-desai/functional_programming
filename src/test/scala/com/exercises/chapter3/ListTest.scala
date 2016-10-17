package com.exercises.chapter3

import org.scalatest._

class ListTest extends FlatSpec with Matchers {
  behavior of "List"

  it should "evaluate pattern matching" in {
    List.evaluate should be(3)
  }

  it should "return tail list" in {
    List.tail(List(1, 2, 3, 4, 5)) should be(List(2, 3, 4, 5))
  }

  it should "setHead to given list" in {
    List.setHead(List(1, 2, 3, 4, 5), 10) should be(List(10, 2, 3, 4, 5))
    List.setHead(Nil, 1) should be(List(1))
  }

  it should "drop first n elements from list" in {
    List.drop(List(1, 2, 3, 4, 5), 0) should be(List(1, 2, 3, 4, 5))
    List.drop(List(1, 2, 3, 4, 5), 2) should be(List(3, 4, 5))
  }

  it should "dropUntil condition is true from list using tail rec function" in {
    List.dropWhile(List(1, 2, 3, 4, 5))(x => x < 3) should be(List(3, 4, 5))
    List.dropWhile(List(1, 2, 3, 4, 5))(x => x < 10) should be(Nil)
  }

  it should "dropUntil condition is true from list using non tail rec function" in {
    List.dropWhile1(List(1, 2, 3, 4, 5))(x => x < 3) should be(List(3, 4, 5))
    List.dropWhile1(List(1, 2, 3, 4, 5))(x => x < 10) should be(Nil)
  }

  it should "return initial elements of list excluding last element using tail rec function" in {
    List.init(List(1, 2, 3, 4, 5)) should be(List(1, 2, 3, 4))
    List.init(List(1)) should be(Nil)
    List.init(Nil) should be(Nil)
  }

  it should "return initial elements of list excluding last element using non tail rec function" in {
    List.init1(List(1, 2, 3, 4, 5)) should be(List(1, 2, 3, 4))
    List.init1(List(1)) should be(Nil)
    List.init1(Nil) should be(Nil)
  }

  it should "calculate product of list with fold left" in {
    List.product(List(1.0, 2.0, 3.0)) should be(6.0)
  }

  it should "calculate product of list with fold right" in {
    List.product1(List(1.0, 2.0, 3.0)) should be(6.0)
  }

  it should "create list using foldRight" in {
    List.create(List(1, 2, 3)) should be(Cons(1, Cons(2, Cons(3, Nil))))
  }

  it should "calculate length of list with fold right" in {
    List.length(List(1, 2, 3)) should be(3)
    List.length(Nil) should be(0)
  }

  it should "calculate length of list with fold left" in {
    List.length1(List(1, 2, 3)) should be(3)
    List.length1(Nil) should be(0)
  }

  it should "implement foldLeft using tailRecursion" in {
    List.foldLeft(List(1, 2, 3), 1)((a, b) => a * b) should be(6)
    List.foldLeft[Int, Int](Nil, 1)((a, b) => a * b) should be(1)
  }

  it should "calculate sum using foldLeft" in {
    List.sum1(List(1, 2, 3)) should be(6)
    List.sum1(Nil) should be(0)
  }

  it should "implement reverse using foldLeft" in {
    List.reverse(List(1, 2, 3)) should be(List(3, 2, 1))
    List.reverse(Nil) should be(Nil)
  }

  it should "implement foldLeft using foldRight" in {
    // (((1-1)-2)-3) = -5
    List.foldLeft1(List(1, 2, 3), 1)((a, b) => a - b) should be(-5)
    // (1-0) = 1
    List.foldLeft1[Int, Int](Nil, 1)((a, b) => a - b) should be(1)
  }

  it should "implement foldRight using foldLeft" in {
    // (1-(2-(3-1))) = 1
    List.foldRight1(List(1, 2, 3), 1)((a, b) => a - b) should be(1)
    // (1-0) = 1
    List.foldRight1[Int, Int](Nil, 1)((a, b) => a - b) should be(1)
  }

  it should "implement append using foldLeft" in {
    List.append(List(1, 2, 3), 4) should be(List(1, 2, 3, 4))
    List.append(Nil, 4) should be(List(4))
  }

  it should "implement append two lists using foldLeft" in {
    List.append(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
    List.append(Nil, List(4)) should be(List(4))
    List.append(List(4), Nil) should be(List(4))
    List.append(Nil, Nil) should be(Nil)
  }

  it should "implement append two lists using foldRight" in {
    List.append1(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
    List.append1(Nil, List(4)) should be(List(4))
    List.append1(List(4), Nil) should be(List(4))
    List.append1(Nil, Nil) should be(Nil)
  }

  it should "implement append two lists using recursion" in {
    List.append2(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))
    List.append2(Nil, List(4)) should be(List(4))
    List.append2(List(4), Nil) should be(List(4))
    List.append2(Nil, Nil) should be(Nil)
  }

  it should "concatenate list of lists into single linear list" in {
    List.concat(List(List(1, 2, 3), List(4, 5, 6))) should be(List(1, 2, 3, 4, 5, 6))
    List.concat(List(List(1), Nil)) should be(List(1))
    List.concat(List(Nil, List(1))) should be(List(1))
    List.concat(List(Nil, Nil)) should be(Nil)
  }

  it should "add 1 to all elements of list using no tail rec" in {
    List.add1ToAll(List(1, 2, 3)) should be(List(2, 3, 4))
    List.add1ToAll(Nil) should be(Nil)
  }

  it should "create string list using toString on each element" in {
    List.toStringList(List(1, 2, 3)) should be(List("1", "2", "3"))
    List.toStringList(Nil) should be(Nil)
  }

  it should "implement map using tail rec function" in {
    List.map(List(1, 2, 3))(x => x + 1) should be(List(2, 3, 4))
    List.map[Int, Int](Nil)(x => x + 1) should be(Nil)
  }

  it should "implement map using foldLeft" in {
    List.map1(List(1, 2, 3))(x => x + 1) should be(List(2, 3, 4))
    List.map1[Int, Int](Nil)(x => x + 1) should be(Nil)
  }

  it should "implement filter" in {
    List.filter(List(1, 2, 3, 4, 5, 6))(x => x % 2 == 0) should be(List(1, 3, 5))
    List.filter(List(1, 2, 3, 4, 5, 6))(_ => true) should be(Nil)
    List.filter[Int](Nil)(x => x / 2 == 1) should be(Nil)
  }

  it should "implement flatMap" in {
    List.flatMap[String, Char](List("abcd", "xyz"))(x => List(x.toCharArray: _*)) should be(List('a', 'b', 'c', 'd', 'x', 'y', 'z'))
  }

  it should "implement filter using flatMap" in {
    List.filter1(List(1, 2, 3, 4, 5, 6))(x => x % 2 == 0) should be(List(1, 3, 5))
    List.filter1(List(1, 2, 3, 4, 5, 6))(_ => true) should be(Nil)
    List.filter1[Int](Nil)(x => x / 2 == 1) should be(Nil)
  }

  it should "get element by index from list" in {
    List.get(List(1, 2, 3), 2) should be(3)

    intercept[IndexOutOfBoundsException](List.get(List(1, 2), 2))
  }

  it should "zip list with index" in {
    List.zipWithIndex(List(1, 2)) should be(List((1, 0), (2, 1)))
    List.zipWithIndex(Nil) should be(Nil)
  }

  it should "add two lists" in {
    List.add(List(1, 2, 3), List(5, 6, 7)) should be(List(6, 8, 10))
    List.add(Nil, Nil) should be(Nil)

    intercept[IllegalArgumentException](List.add(List(1, 2), List(1)))
    intercept[IllegalArgumentException](List.add(List(1), List(1, 2)))
  }

  it should "add two lists using tail rec" in {
    List.add1(List(1, 2, 3), List(5, 6, 7)) should be(List(6, 8, 10))
    List.add1(Nil, Nil) should be(Nil)

    intercept[IllegalArgumentException](List.add1(List(1, 2), List(1)))
    intercept[IllegalArgumentException](List.add1(List(1), List(1, 2)))
  }

  it should "add two lists using non tail rec" in {
    List.add2(List(1, 2, 3), List(5, 6, 7)) should be(List(6, 8, 10))
    List.add2(Nil, Nil) should be(Nil)

    intercept[IllegalArgumentException](List.add2(List(1, 2), List(1)))
    intercept[IllegalArgumentException](List.add2(List(1), List(1, 2)))
  }

  it should "zip two lists with given function applied by using map function" in {
    List.zipWith(List(1, 2, 3), List(5, 6, 7))((a, b) => a + b) should be(List(6, 8, 10))
    List.zipWith[Int, Int](Nil, Nil)((a, b) => a + b) should be(Nil)

    intercept[IllegalArgumentException](List.zipWith(List(1, 2), List(1))((a, b) => a + b))
    intercept[IllegalArgumentException](List.zipWith(List(1), List(1, 2))((a, b) => a + b))
  }

  it should "zip two lists with given function applied by using tail recursive function" in {
    List.zipWith1(List(1, 2, 3), List(5, 6, 7))((a, b) => a + b) should be(List(6, 8, 10))
    List.zipWith1[Int, Int](Nil, Nil)((a, b) => a + b) should be(Nil)

    intercept[IllegalArgumentException](List.zipWith1(List(1, 2), List(1))((a, b) => a + b))
    intercept[IllegalArgumentException](List.zipWith1(List(1), List(1, 2))((a, b) => a + b))
  }

  it should "zip two lists with given function applied by using recursive function" in {
    List.zipWith2(List(1, 2, 3), List(5, 6, 7))((a, b) => a + b) should be(List(6, 8, 10))
    List.zipWith2[Int, Int](Nil, Nil)((a, b) => a + b) should be(Nil)

    intercept[IllegalArgumentException](List.zipWith2(List(1, 2), List(1))((a, b) => a + b))
    intercept[IllegalArgumentException](List.zipWith2(List(1), List(1, 2))((a, b) => a + b))
  }

  it should "check whether list is empty" in {
    List.isEmpty(List(5, 6, 7)) should be(false)
    List.isEmpty(Nil) should be(true)
  }

  it should "get head element of list" in {
    List.head(List(1, 2, 3)) should be(1)

    intercept[NoSuchElementException](List.head(Nil))
  }

  it should "take first n elements from given list" in {
    List.take(List(1, 2, 3), 2) should be(List(1, 2))
    List.take(List(1, 2, 3), 0) should be(Nil)
    List.take(List(1, 2, 3), 10) should be(List(1, 2, 3))
  }

  it should "implement slide function on list" in {
    List.slide(List(1, 2), 3) should be(Cons(List(1, 2), Nil))
    List.slide(List(1, 2, 3), 2) should be(List(List(1, 2), List(2, 3)))
    List.slide(Nil, 2) should be(Cons(Nil, Nil))

    intercept[IllegalArgumentException](List.slide(List(1, 2), 0))
  }

  it should "check whether a list is subSequence of other" in {
    List.hasSubsequence(List(1, 2), List(1, 2)) should be(true)
    List.hasSubsequence(List(1, 2), List(1)) should be(true)
    List.hasSubsequence(List(1, 2), List(2)) should be(true)
    List.hasSubsequence(List(1), Nil) should be(true)

    List.hasSubsequence(List(1, 2), List(1, 2, 3)) should be(false)
    List.hasSubsequence(Nil, List(1)) should be(false)
  }
}
