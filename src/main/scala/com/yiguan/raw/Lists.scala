package com.yiguan.raw

object Lists extends App {

  sealed trait List[+T]

  case object Nil extends List[Nothing]

  case class Cons[T](head: T, tail: List[T]) extends List[T]

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(xs: List[Int]): Int = xs match {
    case Nil => 1
    case Cons(head, tail) => head * product(tail)
  }

  def allTrue(xs: List[Boolean]): Boolean = xs match {
    case Nil => true
    case Cons(head, tail) => head && allTrue(tail)
  }

  def anyTrue(xs: List[Boolean]): Boolean = xs match {
    case Nil => false
    case Cons(head, tail) => head || anyTrue(tail)
  }
  def append(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case Nil => ys
    case Cons(head, tail) => Cons(head, append(tail, ys))
  }

  private val aList: List[Int] = Cons[Int](1, Cons[Int](2, Nil))
  private val bList: List[Boolean] = Cons[Boolean](true, Cons[Boolean](false, Nil))
  private val cList: List[Int] = Cons[Int](3, Cons[Int](4, Nil))

  println(sum(aList))
  println(product(aList))
  println(allTrue(bList))
  println(anyTrue(bList))
  println(append(aList, cList))

}