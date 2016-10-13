package com.exercises.chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //EXERCISE 2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(first, remaining) => remaining
    case Nil => Nil
  }

  //EXERCISE 3
  def drop[A](xs: List[A], ntoDrop: Int): List[A] = {
    @tailrec
    def loop(ys: List[A], n: Int): List[A] =
      if (n == 0) ys
      else loop(tail(ys), n - 1)

    loop(xs, ntoDrop)
  }

  //EXERCISE 4
  def dropWhile[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    @tailrec
    def loop(ys: List[A]): List[A] = ys match {
      case Cons(first, remaining) if predicate(first) => loop(remaining)
      case x => x
    }
    loop(xs)
  }

  def dropWhile1[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    xs match {
      case Nil => Nil
      case Cons(first, tail) if predicate(first) => dropWhile1(tail)(predicate)
      case Cons(first, tail) if !predicate(first) => Cons(first, tail)
    }
  }

  //EXERCISE 5
  def setHead[A](xs: List[A], x: A): List[A] = {
    Cons(x, tail(xs))
  }

  //EXERCISE 6
  def init[A](xs: List[A]): List[A] = {
    @tailrec
    def loop(ys: List[A], acc: List[A]): List[A] = ys match {
      case Cons(first, Cons(last, Nil)) => append(acc, first)
      case Cons(first, remaining) => loop(remaining, append(acc, first))
      case Nil => Nil
    }
    loop(xs, Nil)
  }

  def init1[A](xs: List[A]): List[A] = {
    xs match {
      case Cons(_, Nil) | Nil => Nil
      case Cons(first, tail) => Cons(first, init1(tail))
    }
  }

  //EXERCISE 7
  def product(xs: List[Double]): Double = foldLeft(xs, 1.0)((acc, x) => acc * x)

  //EXERCISE 9
  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    xs match {
      case Cons(first, tail) => f(first, foldRight(tail, z)(f))
      case Nil => z
    }

  def length[A](xs: List[A]): Int = foldRight(xs, 0)((x, l) => l + 1)

  //EXERCISE 10
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(ys: List[A], acc: B): B = ys match {
      case Cons(first, remaining) => loop(remaining, f(acc, first))
      case _ => acc
    }
    loop(xs, z)
  }

  //EXERCISE 12
  def reverse[A](xs: List[A]): List[A] = {
    foldLeft(xs, Nil: List[A])((acc, x) => Cons(x, acc))
  }

  //EXERCISE 13
  def foldLeft1[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(xs), z)((x, acc) => f(acc, x))
  }

  //EXERCISE 14
  def append[A](xs: List[A], toAppend: A): List[A] = {
    foldLeft(reverse(xs), Cons(toAppend, Nil)) { (acc, x) => Cons(x, acc) }
  }

  def append[A](xs: List[A], ys: List[A]): List[A] = {
    foldLeft(ys, xs)((acc, y) => append(acc, y))
  }

  def append1[A](xs: List[A], ys: List[A]): List[A] = {
    foldRight(xs, ys)((x, acc) => Cons(x, acc))
  }

  //EXERCISE 15
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, List[A]())((x, acc) => append(x, acc))

  //EXERCISE 16/17/18
  def map[A, B](xs: List[A])(f: A => B): List[B] = {
    @annotation.tailrec
    def loop(ys: List[A], acc: List[B]): List[B] = ys match {
      case Cons(first, Nil) => append(acc, f(first))
      case Cons(first, tail) => loop(tail, append(acc, f(first)))
      case Nil => Nil
    }
    loop(xs, Nil)
  }

  def map1[A, B](l: List[A])(f: A => B): List[B] = {
    foldLeft(l, List[B]())((acc, x) => append(acc, f(x)))
  }

  //EXERCISE 19
  def filter[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    foldLeft(xs, Nil: List[A]) { (acc, x) =>
      if (predicate(x)) acc
      else append(acc, x)
    }
  }

  //EXERCISE 20
  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = {
    foldLeft(map(xs)(f), Nil: List[B])((acc, x) => append(acc, x))
  }

  //EXERCISE 21
  def filter1[A](xs: List[A])(predicate: A => Boolean): List[A] = {
    flatMap(xs) { x =>
      if (predicate(x)) Nil
      else Cons(x, Nil)
    }
  }

  //EXERCISE 22
  def get[A](xs: List[A], index: Int): A = {
    @annotation.tailrec
    def loop(ys: List[A], i: Int): A = ys match {
      case Cons(first, _) if i == index => first
      case Cons(first, tail) => loop(tail, i + 1)
      case _ => throw new IndexOutOfBoundsException
    }
    loop(xs, 0)
  }

  def zipWithIndex[A](xs: List[A]): List[(A, Int)] = {
    @annotation.tailrec
    def loop(ys: List[A], index: Int, acc: List[(A, Int)]): List[(A, Int)] = ys match {
      case Cons(first, tail) => loop(tail, index + 1, append(acc, (first, index)))
      case Nil => acc
    }
    loop(xs, 0, Nil)
  }

  def add(xs: List[Int], ys: List[Int]): List[Int] = {
    if (length(xs) != length(ys))
      throw new IllegalArgumentException("Cannot add element of lists with diff length")
    else
      map(zipWithIndex(xs))((tuple) => tuple._1 + get(ys, tuple._2))
  }

  //EXERCISE 23
  def mergeUsing[A, B](xs: List[A], ys: List[A])(f: (A, A) => B): List[B] = {
    if (length(xs) != length(ys))
      throw new IllegalArgumentException("Cannot add element of lists with diff length")
    else
      map(zipWithIndex(xs))((tuple) => f(tuple._1, get(ys, tuple._2)))
  }


  def mergeUsing1[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = {
    (xs, ys) match {
      case (Nil, x) => x
      case (ls, Nil) => ls
      case (Cons(l1, l1s), Cons(l2, l2s)) => append(List(f(l1, l2)), mergeUsing1(l1s, l2s)(f))
    }
  }

  //EXERCISE 24
  def isEmpty[A](xs: List[A]): Boolean = length(xs) == 0

  def head[A](xs: List[A]): A = xs match {
    case Cons(head, _) => head
    case Nil => throw new NoSuchElementException("head of empty list")
  }

  def take[A](xs: List[A], ntoTake: Int): List[A] = {
    @tailrec
    def loop(ys: List[A], n: Int, acc: List[A]): List[A] =
      if (length(ys) <= n || isEmpty(ys)) append(acc, ys)
      else if (n == 0) acc
      else loop(tail(ys), n - 1, append(acc, head(ys)))

    loop(xs, ntoTake, Nil)

  }

  def slide[A](xs: List[A], window: Int): List[List[A]] = {
    @tailrec
    def loop(ys: List[A], acc: List[List[A]]): List[List[A]] =
      if (length(ys) < window) acc
      else loop(tail(ys), append[List[A]](acc, take(ys, window)))

    if (window == 0) throw new IllegalArgumentException("Window cannot be 0")
    else if (length(xs) < window) List(xs)
    else loop(xs, Nil)
  }

  def hasSubsequence[A](xs: List[A], sub: List[A]): Boolean =
    isEmpty(sub) || foldLeft(slide(xs, length(sub)), false)((acc, x) => acc || (x == sub))
}