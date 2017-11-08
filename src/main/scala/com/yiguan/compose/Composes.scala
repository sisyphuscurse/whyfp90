package com.yiguan.compose

object Composes extends App {

  sealed trait List[+T]

  case object Nil extends List[Nothing]

  case class Cons[T](head: T, tail: List[T]) extends List[T]

  def foldr[A, E](f: E => A => A)(y: A)(xs: List[E]): A = xs match {
    case Nil => y
    case Cons(head: E, tail: List[E]) => f(head)(foldr(f)(y)(tail))
  }

  def cons[T]: (T) => List[T] => List[T] = (head: T) => (tail: List[T]) => Cons(head, tail)

  def double(n: Int): Int = 2 * n

  def doubleAll(xs: List[Int]): List[Int] = foldr(doubleAndCons)(Nil)(xs)

  def doubleAndCons = fAndCons(double)

  def fAndCons[A, B](f: A => B) = cons[B].compose(f)

  def map[A, B](f: A => B)(xs: List[A]): List[B] = foldr(fAndCons(f))(Nil)(xs)

  private val aList: List[Int] = Cons[Int](1, Cons[Int](2, Nil))

  println(doubleAll(aList))

  println(map(double)(aList))
}
