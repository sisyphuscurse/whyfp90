package com.yiguan

import java.lang.Math.{log10, pow}

import scala.collection.immutable.Stream.cons


object Laziness {

  def abs(one: Double) = if (one < 0) -one else one

  private def next(n: Double)(x: Double) = (x + n / x) / 2

  def repeat[T](f: T => T)(zero: T): Stream[T] = cons(zero, repeat(f)(f(zero)))

  def within(eps: Double): Stream[Double] => Double = {
    case cons(one, tail@cons(other, _)) =>
      println(one, other)
      if (abs(other - one) <= eps) other
      else within(eps)(tail)
  }

  def relative(eps: Double): Stream[Double] => Double = {
    case cons(one, tail@cons(other, _)) =>
      if (abs(one / other - 1) <= eps) other
      else relative(eps)(tail)
  }

  private def easydiff(f: Double => Double, x: Double)(h0: Double) = (f(x + h0) - f(x)) / h0

  private def differentiate(h0: Double)(f: Double => Double, x: Double) = repeat(halve)(h0) map (easydiff(f, x))

  private def halve(x: Double) = x / 2

  private def double(x: Double) = 2 * x

  def elimerror(n: => Long): Stream[Double] => Stream[Double] = {
    case cons(a, tail@cons(b, _)) if a != b =>
      cons((b * power(n) - a) / (power(n) - 1), elimerror(n)(tail))
    case s => s
  }

  private def power(n: Double) = pow(2, n)

  def order: Stream[Double] => Long = {
    case cons(a, cons(b, cons(c, _))) =>
      val x = log2((a - c) / (b - c) - 1).round;
      println(s" n = ${x}");
      x
  }

  def improve(s: Stream[Double]) = elimerror(order(s))(s)

  def complex(x: Double): Double = 3 * pow(x, 3) - 2 * pow(x, 2) - 7

  def superior(s: Stream[Double]) = repeat(improve)(s) map second

  private def second[T]: Stream[T] => T = {
    case cons(_, cons(b, _)) => b
  }

  private def log2(d: Double) = log10(d) / log10(2.0D)

  def easyIntegrate(f: Double => Double)(a: Double)(b: Double) = (f(a) + f(b)) * (b - a) / 2

  def addPair: (Double, Double) => Double = _ + _

  def integrate_slow(f: Double => Double)(a: Double)(b: Double): Stream[Double] = cons(easyIntegrate(f)(a)(b),
    integrate_slow(f)(a)((a + b) / 2)
      .zip(integrate_slow(f)((a + b) / 2)(b))
      .map(addPair.tupled)
  )

  def integrate(f: Double => Double)(a: Double)(b: Double): Stream[Double] = intg(f, a, b, f(a), f(b))

  def intg(f: Double => Double, a: Double, b: Double, fa: Double, fb: Double): Stream[Double] =
    cons((fa + fb) * (b - a) / 2,
      integrate(f)(a)((a + b) / 2)
        .zip(integrate(f)((a + b) / 2)(b))
        .map(addPair.tupled)
    )
}

object Demo extends App {

  import Laziness._

  private val dx = 5
  private val eps = 1e-6
  // samples
  /*
  println("Function complex's differentiation @ 3 is:")
  println(
    within(eps)(differentiate(dx)(complex, 3)),
    within(eps)(improve(differentiate(dx)(complex, 3))),
    within(eps)(improve(improve(differentiate(dx)(complex, 3)))),
    within(eps)(improve(improve(improve(differentiate(dx)(complex, 3)))))
  )
  println(
    relative(eps)(improve(differentiate(dx)(complex, 3))),
    relative(eps)(improve(improve(differentiate(dx)(complex, 3)))),
    relative(eps)(improve(improve(improve(differentiate(dx)(complex, 3)))))
  )
  println("Function double's differentiation @ 3 is:")
  println(
    within(eps)(differentiate(dx)(double, 3))
  )
  println("First 10 estimations")
  println(
    repeat(next(4))(1).take(10).mkString(",")
  )
  println("within 1e-6 repeat next(4) 1")
  println(
    within(eps)(repeat(next(4))(1))
  )
  println("relative 1e-6 repeat next(4) 1")
  println(
    relative(eps)(repeat(next(4))(1))
  )
  */
  println(
    "superior on differentiate",
    within(eps)(superior(differentiate(dx)(complex, 3)))
  )
  println(
    "superior on integrate",
    within(1e-10)(superior(integrate(complex)(0)(3)))
  )

  within(1e-10)(integrate(complex)(0)(3))
}
