package curse.tailrec

import scala.annotation.tailrec

object Demo extends App {

  def factorial(n: BigDecimal): BigDecimal =
    if (n <= 0) 1
    else n * factorial(n - 1)

  println(factorial(1000L))

}

object Demo2 extends App {

  trait TailRec[T]

  case class Value[T](get: T) extends TailRec[T]

  case class Call[T](x: T, y: () => TailRec[T], f: (T, T) => T) extends TailRec[T]

  @tailrec
  def run[T](tailRec: TailRec[T]): T = tailRec match {
    case Value(v) => v
    case Call(x, thunk, f) => thunk() match {
      case Value(v) => f(x, v)
      case Call(y, z, _) => run(Call(f(x, y), z, f))
    }
  }

  def factorial(n: BigDecimal): TailRec[BigDecimal] =
    if (n <= 0) Value(1)
    else Call[BigDecimal](n, () => factorial(n - 1), _ * _)

  println(run(factorial(100000L)))

}

object Demo3 extends App {

  trait TailRec[T] {

    def map[S](f: T => S): TailRec[S] = flatMap(f andThen (Return(_)))

    def flatMap[S](f: T => TailRec[S]): TailRec[S] = FlatMap(this, f)
  }

  //代表无副作用的SubRoutine
  case class Return[T](get: T) extends TailRec[T]

  //代表有副作用的SubRoutine, 在该Demo里，"副作用" 约等于 Function0，但是Function0并非"副作用"
  //如果该副作用是"等待用户在控制台输入", 那么Suspend和resume就更容易理解了
  case class Suspend[T](resume: () => T) extends TailRec[T]

  //代表Sequence of SubRoutine
  case class FlatMap[A, B](subRoutine: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

  //若一个计算任务分解为上面三种形态，那么应该如何解释执行上面三种任务呢？

  @tailrec
  def run[T](subRoutine: TailRec[T]): T = subRoutine match {
    //如果该subRoutine是最后一个SubRoutine
    case Return(v) => v
    case Suspend(r) => r()
    //如果该subRoutine是一系列SubRoutine, 那么如何"粘接"前后两个SubRoutine？
    case FlatMap(theSubRoutine, nextSubRoutineGenerator) => theSubRoutine match {
      case Return(v) => run(nextSubRoutineGenerator(v))
      case Suspend(r) => run(nextSubRoutineGenerator(r()))
      case FlatMap(previousSubRoutine, previousSubRoutineGenerator) =>
        //相当于 FlatMap(
        //              FlatMap(previousSubRoutine, previousSubRoutineGenerator),
        //                       nextSubRoutineGenerator
        //               )
        //      )
        //若等于 FlatMap ( FlatMap (x, f), g )
        //利用结合律转换成为 FlatMap(x, f flatMap g )，
        //相当于通过降阶theSubRoutine, 升阶SubRoutineGenerator消除"大"SubRoutine给Stack造成的压力
        //因此有
        run(
          previousSubRoutine.flatMap(previousSubRoutineGenerator(_).flatMap(nextSubRoutineGenerator))
        )
    }
  }

  object TailRec {
    def unit[A](a: A): TailRec[A] = Return(a)
  }

  import TailRec._

  //重写factorial
  //  def factorial(n: BigDecimal): TailRec[BigDecimal] =
  //    if (n <= 0) unit(1)
  //    else FlatMap(unit(n), ((i: BigDecimal) => factorial(i - 1).map(j => i * j)))
  //其中利用for comprehension可以简写else分支
  def factorial(n: BigDecimal): TailRec[BigDecimal] =
    if (n <= 0) unit(1)
    else for {
      i <- unit(n)
      j <- factorial(i - 1)
    } yield i * j

  println(run(factorial(100000L)))

}
