package com.exercises.chapter1

object Factorial {
  def calculateTailRec(n: Int): Int = {
    @annotation.tailrec
    def loop(acc: Int, n: Int): Int = {
      if (n <= 1) acc
      else loop(n * acc, n - 1)
    }
    loop(1, n)
  }

  def calculateNonTailRec(n: Int): Int = {
    if (n <= 1)
      n
    else
      n * calculateNonTailRec(n - 1)
  }
}