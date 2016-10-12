package com.exercises.chapter2

import org.scalatest.{FlatSpec, Matchers}

class FunctionExamplesTest extends FlatSpec with Matchers{
  "FunctionExample" should "return PartialApplication" in {
    val HBDGreeting = FunctionExamples.partialApplication("Happy birthday", (greetingMessage:String, name:String) => greetingMessage + " " + name)

    HBDGreeting("birthdayBoy") should be("Happy birthday birthdayBoy")
  }

  "FunctionExample" should "return curried function" in {
    def multiplication(a: Int, b: Int): Int = a * b

    val curriedMultiplication = FunctionExamples.curry(multiplication)

    val multiplyBy2 = curriedMultiplication(2)

    multiplyBy2(3) should be(6)
  }
  
  "FunctionExample" should "return uncurried function" in {
    def greetings(wishes: String) = (personToWish: String) => wishes + " " + personToWish

    val greetingCard = FunctionExamples.uncurry(greetings)

    greetingCard("Happy Birthday", "birthdayBoy") should be("Happy Birthday birthdayBoy")
  }

  "FunctionExample" should "compose functions" in {
    def increment(n: Int) = n + 1
    def square(n: Int) = n * n

    val incrementedSquare = FunctionExamples.compose[Int, Int, Int](increment, square)
    incrementedSquare(2) should be(5)

    val squaredIncrementation = FunctionExamples.compose[Int, Int, Int](square, increment)
    squaredIncrementation(2) should be(9)
  }
}
