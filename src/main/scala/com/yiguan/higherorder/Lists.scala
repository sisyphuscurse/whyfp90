package com.yiguan.higherorder

object Lists extends App {

  sealed trait List[+T]

  case object Nil extends List[Nothing]

  case class Cons[T](head: T, tail: List[T]) extends List[T]

  def foldr[A, E](f: (E, A) => A)(y: A)(xs: List[E]): A = xs match {
    case Nil => y
    case Cons(head: E, tail: List[E]) => f(head, foldr(f)(y)(tail))
  }

  private def plus(a: Int, b: Int) = a + b

  private def multiply(a: Int, b: Int) = a * b

  private def and(a: Boolean, b: Boolean) = a && b

  private def or(a: Boolean, b: Boolean) = a || b

  private def cons(x: Int, zs: List[Int]): List[Int] = Cons(x, zs)

  def sum(xs: List[Int]): Int = foldr(plus)(0)(xs)

  def product(xs: List[Int]): Int = foldr(multiply)(1)(xs)

  def allTrue(xs: List[Boolean]): Boolean = foldr(and)(true)(xs)

  def anyTrue(xs: List[Boolean]): Boolean = foldr(or)(false)(xs)

  def append(xs: List[Int], ys: List[Int]): List[Int] = foldr(cons)(ys)(xs)


  private val aList: List[Int] = Cons[Int](1, Cons[Int](2, Nil))
  private val bList: List[Boolean] = Cons[Boolean](true, Cons[Boolean](false, Nil))
  private val cList: List[Int] = Cons[Int](3, Cons[Int](4, Nil))

  println(sum(aList))
  println(product(aList))
  println(allTrue(bList))
  println(anyTrue(bList))
  println(append(aList, cList))

}