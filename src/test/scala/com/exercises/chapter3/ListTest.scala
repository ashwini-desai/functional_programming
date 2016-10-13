package com.exercises.chapter3

import org.scalatest._
import scala.collection.immutable.{List => CList, Nil => CNil}

class ListTest extends FlatSpec with Matchers {
  behavior of "List"

  it should "create custom List from scala collection list" in {
    from(CList(1, 2)) should be(Cons(1, Cons(2, Nil)))
    from(CNil) should be(Nil)
  }

  it should "return tail list" in {
    List.tail(from(CList(1, 2, 3, 4, 5))) should be(from(CList(2, 3, 4, 5)))
  }

  it should "drop first n elements from list" in {
    List.drop(from(CList(1, 2, 3, 4, 5)), 0) should be(from(CList(1, 2, 3, 4, 5)))
    List.drop(from(CList(1, 2, 3, 4, 5)), 2) should be(from(CList(3, 4, 5)))
  }

  it should "dropUntil condition is true from list" in {
    List.dropUntil(from(CList(1, 2, 3, 4, 5)))(x => x < 3) should be(from(CList(3, 4, 5)))
    List.dropUntil(from(CList(1, 2, 3, 4, 5)))(x => x < 10) should be(Nil)
  }

  it should "setHead to given list" in {
    List.setHead(from(CList(1, 2, 3, 4, 5)), 10) should be(from(CList(10, 2, 3, 4, 5)))
    List.setHead(Nil, 1) should be(from(CList(1)))
  }

  it should "return initial elements of list excluding last element" in {
    List.init(from(CList(1, 2, 3, 4, 5))) should be(from(CList(1, 2, 3, 4)))
    List.init(from(CList(1))) should be(Nil)
    List.init(Nil) should be(Nil)
  }

  it should "calculate product of list with fold right" in {
    List.product(from(CList(1.0, 2.0, 3.0))) should be(6.0)
  }

  it should "calculate length of list with fold right" in {
    List.length(from(CList(1, 2, 3))) should be(3)
    List.length(Nil) should be(0)
  }

  it should "implement foldLeft using tailRecursion" in {
    List.foldLeft(from(CList(1, 2, 3)), 1)((a, b) => a * b) should be(6)
    List.foldLeft[Int, Int](Nil, 1)((a, b) => a * b) should be(1)
  }

  it should "implement reverse using foldLeft" in {
    List.reverse(from(CList(1, 2, 3))) should be(from(CList(3, 2, 1)))
    List.reverse(Nil) should be(Nil)
  }

  it should "implement foldLeft using foldRight" in {
    List.foldLeft1(from(CList(1, 2, 3)), 1)((a, b) => a * b) should be(6)
    List.foldLeft1[Int, Int](Nil, 1)((a, b) => a * b) should be(1)
  }

  it should "implement append using foldLeft" in {
    List.append(from(CList(1, 2, 3)), 4) should be(from(CList(1, 2, 3, 4)))
    List.append(Nil, 4) should be(from(CList(4)))
  }

  it should "implement append two lists using foldLeft" in {
    List.append(from(CList(1, 2, 3)), from(CList(4, 5, 6))) should be(from(CList(1, 2, 3, 4, 5, 6)))
    List.append(Nil, from(CList(4))) should be(from(CList(4)))
    List.append(from(CList(4)), Nil) should be(from(CList(4)))
    List.append(Nil, Nil) should be(Nil)
  }

  it should "implement map" in {
    List.map(from(CList(1, 2, 3)))(x => x + 1) should be(from(CList(2, 3, 4)))
    List.map[Int, Int](Nil)(x => x + 1) should be(Nil)
  }

  it should "implement filter" in {
    List.filter(from(CList(1, 2, 3, 4, 5, 6)))(x => x % 2 == 0) should be(from(CList(1, 3, 5)))
    List.filter(from(CList(1, 2, 3, 4, 5, 6)))(_ => true) should be(Nil)
    List.filter[Int](Nil)(x => x / 2 == 1) should be(Nil)
  }

  it should "implement flatMap" in {
    List.flatMap[String, Char](from(CList("abcd", "xyz")))(x => from(x.toCharArray.toList)) should be(from(CList('a', 'b', 'c', 'd', 'x', 'y', 'z')))
  }

  it should "implement filter using flatMap" in {
    List.filter1(from(CList(1, 2, 3, 4, 5, 6)))(x => x % 2 == 0) should be(from(CList(1, 3, 5)))
    List.filter1(from(CList(1, 2, 3, 4, 5, 6)))(_ => true) should be(Nil)
    List.filter1[Int](Nil)(x => x / 2 == 1) should be(Nil)
  }

  it should "get element by index from list" in {
    List.get(from(CList(1, 2, 3)), 2) should be(3)

    intercept[IndexOutOfBoundsException](List.get(from(CList(1, 2)), 2))
  }

  it should "zip list with index" in {
    List.zipWithIndex(from(CList(1, 2))) should be(from(CList((1, 0), (2, 1))))
    List.zipWithIndex(Nil) should be(Nil)
  }

  it should "add two lists" in {
    List.add(from(CList(1, 2, 3)), from(CList(5, 6, 7))) should be(from(CList(6, 8, 10)))
    List.add(Nil, Nil) should be(Nil)

    intercept[IllegalArgumentException](List.add(from(CList(1, 2)), from(CList(1))))
  }

  it should "merge two lists with given function applied" in {
    List.mergeWith(from(CList(1, 2, 3)), from(CList(5, 6, 7)))((a, b) => a + b) should be(from(CList(6, 8, 10)))
    List.mergeWith[Int, Int](Nil, Nil)((a, b) => a + b) should be(Nil)

    intercept[IllegalArgumentException](List.mergeWith(from(CList(1, 2)), from(CList(1)))((a, b) => a + b))
  }

  it should "check whether list is empty" in {
    List.isEmpty(from(CList(5, 6, 7))) should be(false)
    List.isEmpty(Nil) should be(true)
  }

  it should "get head element of list" in {
    List.head(from(CList(1, 2, 3))) should be(1)

    intercept[NoSuchElementException](List.head(Nil))
  }

  it should "take first n elements from given list" in {
    List.take(from(CList(1, 2, 3)), 2) should be(from(CList(1, 2)))
    List.take(from(CList(1, 2, 3)), 0) should be(Nil)
    List.take(from(CList(1, 2, 3)), 10) should be(from(CList(1, 2, 3)))
  }

  it should "implement slide function on list" in {
    List.slide(from(CList(1, 2)), 3) should be(Cons(from(CList(1, 2)), Nil))
    List.slide(from(CList(1, 2, 3)), 2) should be(from(CList(from(CList(1, 2)), from(CList(2, 3)))))
  }

  it should "check whether a list is subSequence of other" in {
    List.hasSubsequence(from(CList(1, 2)), from(CList(1, 2))) should be(true)
    List.hasSubsequence(from(CList(1, 2)), from(CList(1))) should be(true)
    List.hasSubsequence(from(CList(1, 2)), from(CList(2))) should be(true)

    List.hasSubsequence(from(CList(1, 2)), from(CList(1, 2, 3))) should be(false)
    List.hasSubsequence(Nil, from(CList(1))) should be(false)
    List.hasSubsequence(from(CList(1)), Nil) should be(false)
  }

  //for test utils
  def from[A](xs: CList[A]): List[A] = {
    xs.foldLeft[List[A]](Nil)((acc, x) => List.append(acc, x))
  }
}
