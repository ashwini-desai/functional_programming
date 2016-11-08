package com.exercises.chapter3

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object Empty extends Tree[Nothing]

object Tree {
  def apply[A](): Tree[A] = Empty

  def apply[A](value: A): Tree[A] = Leaf(value)

  def apply[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  //EXERCISE 25
  def size[A](tree: Tree[A]): Int = tree match {
    case Empty => 0
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  //EXERCISE 26
  def max(tree: Tree[Int]): Int = tree match {
    case Empty => throw new UnsupportedOperationException("Operation is not supported on empty list")
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  //EXERCISE 27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Empty => throw new UnsupportedOperationException("Operation is not supported on empty list")
    case Leaf(_) => 0
    case Branch(Leaf(_), right) => 1 + depth(right)
    case Branch(left, Leaf(_)) => depth(left) + 1
    case Branch(left, right) => (depth(left) + 1) max (1 + depth(right))
  }

  //EXERCISE 28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Empty => Empty
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }


  //EXERCISE 29
  def fold[A, B](tree: Tree[A], seed: B)(f: (A, B) => B)(g: (B, B) => B): B = (tree: Tree[A]) match {
    case Empty => seed
    case Leaf(a) => f(a, seed)
    case Branch(left: Tree[A], right: Tree[A]) => g(fold(left, seed)(f)(g), fold(right, seed)(f)(g))
  }

  def size1[A](tree: Tree[A]): Int =
    fold(tree, 0) { case (_, acc) => 1 + acc } { case (leftSize, rightSize) => 1 + leftSize + rightSize }

  def max1(tree: Tree[Int]): Int =
  fold(tree, Int.MinValue) { case (value, acc) => value max acc } { case (leftMax, rightMax) => leftMax max rightMax }

  def depth1[A](tree: Tree[A]): Int =
    fold(tree, 0) { case (_, _) => 0 } { case (leftDepth, rightDepth) => 1 + (leftDepth max rightDepth) }

  def map1[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree, Empty) { case (value, _) => Leaf(f(value)) } {case (left, right) => Branch(left, right)
    }

}
