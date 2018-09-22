package com.yiguan.compose

object Composes extends App {

  sealed trait List[+T]

  case object Nil extends List[Nothing]

  case class Cons[T](head: T, tail: List[T]) extends List[T]
  private def cons[T]: (T) => List[T] => List[T] = (head: T) => (tail: List[T]) => Cons(head, tail)

  def foldr[A, E](f: E => A => A)(y: A)(xs: List[E]): A = xs match {
    case Nil => y
    case Cons(head: E, tail: List[E]) => f(head)(foldr(f)(y)(tail))
  }

  def doubleAll(xs: List[Int]): List[Int] = foldr(doubleAndCons)(Nil)(xs)

  private def doubleAndCons = fAndCons(double)

  private def double(n: Int): Int = 2 * n

  private def fAndCons[A, B](f: A => B) = cons[B].compose(f)

  private val aList: List[Int] = Cons[Int](1, Cons[Int](2, Nil))
  println(doubleAll(aList))

  def map[A, B](f: A => B)(xs: List[A]): List[B] = foldr(cons compose f)(Nil)(xs)
  println(map(double)(aList))

}
