package com.exercises.chapter2

//EXERCISE 1
object Fibonacci {
  def calculateTailRec(n: Int): Int = {
    @annotation.tailrec
    def loop(first: Int, second: Int, index: Int): Int = {
      if (index == n) first
      else loop(second, first + second, index + 1)
    }
    loop(0, 1, 0)
  }

  def calculateNonTailRec(n: Int): Int = {
    if (n == 1) 0
    else if (n == 2) 1
    else calculateNonTailRec(n - 1) + calculateNonTailRec(n - 2)
  }
}
