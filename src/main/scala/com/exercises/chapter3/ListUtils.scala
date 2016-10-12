package com.exercises.chapter3

import scala.annotation.tailrec
import scala.collection.immutable.List

object ListUtils {

  //EXERCISE 2
  def tail[A](xs: List[A]): List[A] = xs match {
    case first :: remaining => remaining
    case Nil => Nil
  }

  //EXERCISE 3
  def drop[A](xs: List[A], ntoDrop: Int): List[A] = {
    @tailrec
    def loop(ys: List[A], n: Int): List[A] =
      if (n == 0) ys
      else loop(ys.tail, n - 1)

    loop(xs, ntoDrop)
  }

  //EXERCISE 4
  def dropUntil[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    @tailrec
    def loop(ys: List[A]): List[A] = ys match {
      case first :: remaining if predicate(first) => loop(remaining)
      case x => x
    }
    loop(xs)
  }

  //EXERCISE 5
  def setHead[A](xs: List[A], x: A): List[A] = {
    x :: tail(xs)
  }

  //EXERCISE 6
  def init[A](xs: List[A]): List[A] = xs match {
    case first :+ last => first
    case Nil => Nil
  }

  //EXERCISE 7
  def product(xs: List[Double]): Double = xs.foldRight(1.0)((x, acc) => acc * x)

  //EXERCISE 9
  def length[A](xs: List[A]): Int = xs.foldRight(0)((x, l) => l + 1)

  //EXERCISE 10
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(ys: List[A], acc: B): B = ys match {
      case first :: remaining => loop(remaining, f(acc, first))
      case _ => acc
    }
    loop(xs, z)
  }

  //EXERCISE 12
  def reverse[A](xs: List[A]): List[A] = {
    foldLeft(xs, List.empty[A])((acc, x) => x +: acc)
  }

  //EXERCISE 13
  def foldLeft1[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    reverse(xs).foldRight(z)((x, acc) => f(acc, x))
  }

  //EXERCISE 14
  def append[A](xs: List[A], x: A): List[A] = {
    foldLeft(xs, List(x)) { (acc, x) =>
      acc match {
        case first :+ last => first :+ x :+ last
        case Nil => Nil
      }
    }
  }

  //EXERCISE 15
  def append[A](xs: List[A], ys: List[A]): List[A] = {
    foldLeft(ys, xs)((acc, y) => acc :+ y)
  }

  //EXERCISE 16/17/18
  def map[A, B](xs: List[A])(f: A => B): List[B] = {
    for {
      x <- xs
    } yield f(x)
  }

  //EXERCISE 19
  def filter[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    foldLeft(xs, List.empty[A]) { (acc, x) =>
      if (predicate(x)) acc
      else acc :+ x
    }
  }

  //EXERCISE 20
  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = {
    foldLeft(map(xs)(f), List.empty[B])((acc, x) => append(acc, x))
  }

  //EXERCISE 21
  def filter1[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    flatMap(xs) { x =>
      if (predicate(x)) List.empty[A]
      else List(x)
    }
  }

  //EXERCISE 22
  def add(xs: List[Int], ys: List[Int]): List[Int] = {
    if (xs.length != ys.length)
      throw new IllegalArgumentException
    else
      map(xs.zipWithIndex)((tuple) => tuple._1 + ys(tuple._2))
  }

  //EXERCISE 23
  def mergeWith[A, B](xs: List[A], ys: List[A])(f: (A, A) => B): List[B] = {
    if (xs.length != ys.length)
      throw new IllegalArgumentException
    else
      map(xs.zipWithIndex)((tuple) => f(tuple._1, ys(tuple._2)))
  }

  //EXERCISE 24
  def take[A](xs: List[A], ntoTake: Int): List[A] = {
    @tailrec
    def loop(ys: List[A], n: Int, acc: List[A]): List[A] =
      if (ys.length <= n || ys.isEmpty) acc ++ ys
      else if (n == 0) acc
      else loop(tail(ys), n - 1, acc :+ ys.head)

    loop(xs, ntoTake, List.empty)

  }

  def slide[A](xs: List[A], window: Int): List[List[A]] = {
    @tailrec
    def loop(ys: List[A], acc: List[List[A]]): List[List[A]] =
      if (ys.length < window) acc
      else loop(tail(ys), acc :+ take(ys, window))

    if (window == 0) List.empty
    else if (xs.length < window) List(xs)
    else loop(xs, List.empty)
  }

  def hasSubsequence[A](xs: List[A], sub: List[A]): Boolean =
    foldLeft(slide(xs, sub.length), false)((acc, x) => acc || (x == sub))
}