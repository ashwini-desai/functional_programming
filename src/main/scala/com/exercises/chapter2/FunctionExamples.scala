package com.exercises.chapter2

object FunctionExamples {

  //EXERCISE 3
  def partialApplication[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a,b)

  //EXERCISE 4
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => partialApplication(a, f)

  //EXERCISE 5
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  //EXERCISE 6
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
