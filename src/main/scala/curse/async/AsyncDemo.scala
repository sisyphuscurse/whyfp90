package net.imadz.fpinscala

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AsyncDemo extends App {

  type Par[A] = Future[A]

  object Par {
    def unit[A](a: A): Par[A] = Future.successful(a)
  }

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]) = FlatMap(this, f)

    def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
  }

  case class Return[A](get: A) extends Async[A]

  case class Suspend[A](resume: Par[A]) extends Async[A]

  case class FlatMap[A, B](subRoutine: Async[A], f: A => Async[B]) extends Async[B]

  import Par._

  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(Return(v), f) => step(f(v))
    case FlatMap(FlatMap(y, g), f) => step(y.flatMap(g(_).flatMap(f)))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(v) => unit(v)
    case Suspend(par) => par
    case FlatMap(subRoutine, f) => subRoutine match {
      case Suspend(par) => par.flatMap(x => run(f(x)))
      case _ => sys.error("Impossible to be here.")
    }
  }

  def factorial(n: BigDecimal): Async[BigDecimal] =
    if (n <= 0) Return(1)
    else if (n % 2 == 0) Suspend[BigDecimal] {
      Future.successful {
        println("Press anything to continue")
        Console.readLine()
      }.flatMap { _ =>
        run(FlatMap(Return(n), (i: BigDecimal) => factorial(n - 1).map(_ * i)))
      }
    }
    else
      FlatMap(Return(n), (i: BigDecimal) => factorial(n - 1).map(_ * i))

  def factorialWithoutSuspend(n: BigDecimal): Async[BigDecimal] =
    if (n <= 0) Return(1)
    else
      FlatMap(Return(n), (i: BigDecimal) => factorialWithoutSuspend(n - 1).map(_ * i))

  run(factorial(3)).foreach(println)

  run(factorialWithoutSuspend(100000L)).foreach { x =>
    println(x)
    sys.exit
  }

  Thread.sleep(100000L)
}
