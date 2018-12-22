package curse.free

import scala.annotation.tailrec

object FreeDemo {

  trait Monad[F[_]] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]
  }

  sealed trait Free[F[_], A] {
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))

    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  }

  object Free {

    def compose[F[_], A, B, C](f: A => Free[F, B])(g: B => Free[F, C]): A => Free[F, C] =
      a => f(a).flatMap(g)
  }

  case class Return[F[_], A](get: A) extends Free[F, A]

  case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](sub: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type lamdba[A] = Free[F, A]})#lamdba] {
    override def flatMap[A, B](fa: Free[F, A])(f: (A) => Free[F, B]) = fa flatMap f

    override def map[A, B](fa: Free[F, A])(f: (A) => B) = flatMap(fa)(f andThen (unit(_)))

    override def unit[A](a: => A) = Return(a)
  }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(x) => x
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(y) => runTrampoline(f(y))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) => runTrampoline(y flatMap {
        g(_) flatMap f
      })
    }
  }

  @tailrec
  def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(Return(x), f) => step(f(x))
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(f(_).flatMap(g)))
    case _ => free
  }

  def run[F[_], A](a: Free[F, A])(implicit m: Monad[F]): F[A] = step(a) match {
    case Return(x) => m.unit(x)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => m.flatMap(r)(y => run(f(y)))
    case _ => sys.error("Impossible to be here, since step eliminates these cases.")
  }

}
