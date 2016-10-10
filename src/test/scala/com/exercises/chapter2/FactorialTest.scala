package com.exercises.chapter2

import org.scalatest._

class FactorialTest extends FlatSpec with Matchers {
  "Factorial tail recursive" should "calculate" in {
    Factorial.calculateTailRec(1) should be (1)
    Factorial.calculateTailRec(3) should be (6)
    Factorial.calculateTailRec(4) should be (24)
  }

  "Factorial non tail recursive" should "calculate" in {
    Factorial.calculateNonTailRec(1) should be (1)
    Factorial.calculateNonTailRec(3) should be (6)
    Factorial.calculateNonTailRec(4) should be (24)
  }
}
