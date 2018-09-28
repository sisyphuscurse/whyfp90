package curse.console

import curse.free.FreeDemo._

object ConsoleIODemo extends App {

  sealed trait Console[T]

  case object ReadLine extends Console[Option[String]]

  case class Println(line: String) extends Console[Unit]

  import curse.free._

  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(Println(line))


  val f1: Free[Console, Option[String]] = for {
    _ <- println("I can only inteact with the console.")
    ln <- readLn
  } yield ln

  val monad: Monad[Console] = new Monad[Console] {

    override def flatMap[A, B](fa: Console[A])(f: A => Console[B]): Console[B] = fa match {
      case ReadLine => ???
      case Println(line) => ???
    }

    override def map[A, B](fa: Console[A])(f: A => B): Console[B] = ???

    override def unit[A](a: => A): Console[A] = ???
  }

  run[Console, Option[String]](f1)(monad)

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(v) => G.unit(v)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(a)(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }


}
