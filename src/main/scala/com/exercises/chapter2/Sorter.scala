package com.exercises.chapter2

//EXERCISE 2
object Sorter {
  def isSorted[A](xs: Array[A], isGreaterThan: (A,A) => Boolean): Boolean = {
    xs.sliding(2).forall(window => isGreaterThan(window(0), window(1)))
  }
}
