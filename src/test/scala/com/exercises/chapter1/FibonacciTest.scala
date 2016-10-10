package com.exercises.chapter1

import org.scalatest._

class FibonacciTest extends FlatSpec with Matchers {
  "Fibonacci tail recursive" should "calculate" in {
    Fibonacci.calculateTailRec(1) should be (1)
    Fibonacci.calculateTailRec(3) should be (2)
    Fibonacci.calculateTailRec(4) should be (3)
    Fibonacci.calculateTailRec(6) should be (8)
  }

  "Fibonacci non tail recursive" should "calculate" in {
    Fibonacci.calculateTailRec(1) should be (1)
    Fibonacci.calculateTailRec(3) should be (2)
    Fibonacci.calculateTailRec(4) should be (3)
    Fibonacci.calculateTailRec(6) should be (8)
  }
}
