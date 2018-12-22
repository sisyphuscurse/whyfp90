package com.yiguan

import java.lang.Math.pow

import scala.collection.immutable.Stream.cons


object Laziness extends App {

  def abs(one: Double) = if (one < 0) -one else one

  def next(n: Double)(x: Double) = (x + n / x) / 2

  def repeat(f: Double => Double)(zero: Double): Stream[Double] = cons(zero, repeat(f)(f(zero)))

  def within(eps: Double): Stream[Double] => Double = {
    case cons(one, tail@cons(other, _)) =>
      if (abs(other - one) <= eps) other
      else within(eps)(tail)
  }

  def relative(eps: Double): Stream[Double] => Double = {
    case cons(one, tail@cons(other, _)) =>
      if (abs(one / other - 1) <= eps) other
      else relative(eps)(tail)
  }

  def sqrt(n: Double, eps: Double, zero: Double) = relative(eps)(repeat(next(n))(zero))

  println("First 10 estimations")
  println(
    repeat(next(4))(1).take(10).mkString(",")
  )
  println("within 1e-6 repeat next(4) 1")
  private val eps = 1e-6
  println(
    within(eps)(repeat(next(4))(1))
  )
  println("relative 1e-6 repeat next(4) 1")
  println(
    relative(eps)(repeat(next(4))(1))
  )

  def easydiff(f: Double => Double, x: Double)(h0: Double) = (f(x + h0) - f(x)) / h0

  def differentiate(h0: Double)(f: Double => Double, x: Double) = repeat(halve)(h0) map (easydiff(f, x))

  def halve(x: Double) = x / 2

  def double(x: Double) = 2 * x

  println("Function double's differentiation @ 3 is:")
  private val dx = 5
  println(
    within(eps)(differentiate(dx)(double, 3))
  )

  def elimerror(n: Long): Stream[Double] => Stream[Double] = {
    case cons(a, tail@cons(b, _)) =>
      cons((b * power(n) - a) / power(n), elimerror(n)(tail))
  }

  private def power(n: Double) = {
    pow(2, n)
  }

  def order: Stream[Double] => Long = {
    case cons(a, cons(b, cons(c, _))) => Math.log((a - c) / (b - c) - 1).round
  }

  def improve(s: Stream[Double]) = elimerror(order(s))(s)

  def complex(x: Double): Double = double(x)//3 * pow(x, 3) - 2 * pow(x, 2) - 7
  println("Function complex's differentiation @ 3 is:")
  println(
    within(eps)(improve(differentiate(dx)(complex, 3)))
  )
  println(
    within(eps)(improve(improve(differentiate(dx)(complex, 3))))
  )
  println(
    within(eps)(improve(improve(improve(differentiate(dx)(complex, 3)))))
  )
  /*
  println(
    relative(eps)(improve(differentiate(dx)(complex, 3)))
  )
  println(
    relative(eps)(improve(improve(differentiate(dx)(complex, 3))))
  )
  println(
    relative(eps)(improve(improve(improve(differentiate(dx)(complex, 3))))),
    relative(eps)(improve(improve(improve(improve(differentiate(dx)(complex, 3)))))),
    relative(eps)(improve(improve(improve(improve(improve(differentiate(dx)(complex, 3)))))))
  )
  */
}
