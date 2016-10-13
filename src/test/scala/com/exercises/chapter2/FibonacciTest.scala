package com.exercises.chapter2

import org.scalatest._

class FibonacciTest extends FlatSpec with Matchers {
  behavior of "Fibonacci"

  it should "calculate using tail recursive function" in {
    Fibonacci.calculateTailRec(1) should be (1)
    Fibonacci.calculateTailRec(3) should be (2)
    Fibonacci.calculateTailRec(4) should be (3)
    Fibonacci.calculateTailRec(6) should be (8)
  }

  it should "calculate using non tail recursive function" in {
    Fibonacci.calculateTailRec(1) should be (1)
    Fibonacci.calculateTailRec(3) should be (2)
    Fibonacci.calculateTailRec(4) should be (3)
    Fibonacci.calculateTailRec(6) should be (8)
  }
}
