package curse.console

import curse.free.FreeDemo._

object ConsoleIODemo extends App {

  sealed trait Console[T] {
    def toThunk(): () => T
  }

  case object ReadLine extends Console[Option[String]] {
    override def toThunk(): () => Option[String] = () => run

    def run(): Option[String] = try Some(Console.readLine) catch {
      case e: Exception => None
    }
  }

  case class Println(line: String) extends Console[Unit] {
    override def toThunk(): () => Unit = () => println(line)
  }

  import curse.free._

  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(Println(line))


  val f1: Free[Console, Option[String]] = for {
    _ <- printLn("I can only inteact with the console.")
    ln <- readLn
  } yield ln

  /*
  val monad: Monad[Console] = new Monad[Console] {

    override def flatMap[A, B](fa: Console[A])(f: A => Console[B]): Console[B] = fa match {
      case ReadLine => ???
      case Println(line) => ???
    }

    override def map[A, B](fa: Console[A])(f: A => B): Console[B] = ???

    override def unit[A](a: => A): Console[A] = ???
  }

    does not work
    run[Console, Option[String]](f1)(monad)
  */
  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(v) => G.unit(v)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }

  val consoleToFunction0: ~>[Console, Function0] = new ~>[Console, Function0] {
    override def apply[A](f: Console[A]): () => A = f.toThunk()
  }

  val F: Monad[Function0] = new Monad[Function0] {
    override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B = () =>
      f(fa())()

    override def map[A, B](fa: () => A)(f: A => B): () => B = () => f(fa())

    override def unit[A](a: => A): () => A = () => a
  }

  val s: () => Option[String] = runFree(f1)(consoleToFunction0)(F)

  println(s())


  type FF[A] = () => A
  type FreeMonad[A] = Free[FF, A]

  val fft: ~>[FF, FreeMonad] = new ~>[FF, FreeMonad] {
    override def apply[A](f: FF[A]): FreeMonad[A] = Suspend(() => f())
  }

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val toFreeG: F ~> FreeG = new ~>[F, FreeG] {
      override def apply[A](f: F[A]): FreeG[A] = Suspend(fg(f))
    }
    runFree(f)(toFreeG)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A =
    runTrampoline(translate(a)(consoleToFunction0))
}
