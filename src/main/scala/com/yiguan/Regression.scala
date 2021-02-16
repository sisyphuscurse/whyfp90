package com.yiguan

import java.lang.Math.pow

object Regression extends App {

  val x_data = List(338.0D, 333.0D, 328.0D, 207.0D, 226.0D, 25.0D, 179.0D, 60.0D, 208.0D, 606.0D)
  val y_data = List(640.0D, 633.0D, 619.0D, 393.0D, 428.0D, 27.0D, 193.0D, 66.0D, 226.0D, 1591.0D)

  def f(a: Double, b: Double)(x: Double) = a * x + b

  // iteration for minimum
  def loss(f: Double => Double): Double = (x_data zip y_data).foldLeft(0D) { case (acc, (x, y)) =>
    pow((y - f(x)), 2) + acc
  } / x_data.length

  val bs = -200D to -100D by 0.01D toStream
  val as = -5.0D to 5.0D by 0.01 toStream

  val loss_matrix = for {
    a <- as
    b <- bs
  } yield a -> b -> loss(f(a, b))

  println(
    loss_matrix.reduce[((Double, Double), Double)] {
      case (p0@((_, _), l0), p1@((_, _), l1)) =>
        if (l0 < l1) p0 else p1
    }
  )

  val a0 = -4D
  val b0 = -120D
  val lr = 1D

  def partial_diff_on_a(a: Double, b: Double)(x: Double, y: Double): Double = 2D * (y - (f(a, b)(x))) * x

  def partial_diff_on_b(a: Double, b: Double)(x: Double, y: Double): Double = 2D * (y - (f(a, b)(x))) * 1D

  case class Iteration(a: Double = 0D, b: Double = 0D, lr_a: Double = 0D, lr_b: Double = 0D)

  import Laziness._

  def gradient: Iteration => Iteration = {
    case Iteration(a0, b0, lra0, lrb0) =>
      val a_grade = sum_partial_diff(a0, b0)(partial_diff_on_a)
      val b_grade = sum_partial_diff(a0, b0)(partial_diff_on_b)

      val lra1 = next_learning_rate(a_grade)(lra0)
      val lrb1 = next_learning_rate(b_grade)(lrb0)

      val a1 = a0 - lr / Math.sqrt(lra1) * a_grade
      val b1 = b0 - lr / Math.sqrt(lrb1) * b_grade
      Iteration(a1, b1, lra1, lrb1)
  }

  private def next_learning_rate(sum_partial_diff: Double)(lr: Double): Double = lr + pow(sum_partial_diff, 2.0D)

  private def sum_partial_diff(a: Double, b: Double)(f: (Double, Double) => (Double, Double) => Double): Double =
    (x_data zip y_data).foldLeft(0D) { case (sum, (x, y)) => sum - f(a, b)(x, y) }

  val its: Stream[Iteration] = repeat(gradient)(Iteration())

  println(
    its.drop(100000).take(1).head
  )

}

object VectorDemo extends App with Regressionable {
  private def toVector: ((Double, Double)) => Vector[Double] = t => Vector(t._1, t._2)

  val x_data = List(338.0D, 333.0D, 328.0D, 207.0D, 226.0D, 25.0D, 179.0D, 60.0D, 208.0D, 606.0D)
  val y_data = List(640.0D, 633.0D, 619.0D, 393.0D, 428.0D, 27.0D, 193.0D, 66.0D, 226.0D, 1591.0D)

  override def initialFactors = Vector[Double](-4D, -120D)

  override def initialLearningRates = Vector[Double](1D, 1D)

  override def trainingDataSet = x_data zip y_data map (toVector)

  override def derivativeFunctions = Vector[Factors => TrainingData => Double](
    derivativeFunctionOnA,
    derivativeFunctionOnB
  )

  def f(a: Double, b: Double)(x: Double) = a * x + b

  private def derivativeFunctionOnA(factors: Factors)(trainingData: TrainingData): Double = {
    val y = trainingData(1)
    val x = trainingData(0)
    val a = factors(0)
    val b = factors(1)
    2D * (y - (f(a, b)(x))) * x
  }

  private def derivativeFunctionOnB(factors: Factors)(trainingData: TrainingData): Double = {
    val y = trainingData(1)
    val x = trainingData(0)
    val a = factors(0)
    val b = factors(1)
    2D * (y - (f(a, b)(x))) * 1D
  }

  //println(
  //its.drop(100000).take(1).head
  //)

  import vegas._
  import vegas.render.WindowRenderer._

  Vegas
    .layered("Regression")
    .withData(
      its.take(100).map(factors => Map("x" -> factors(0), "y" -> factors(1)))
    ).
    withLayers(
      Layer().
        mark(Point).
        encodeX(field = "x", value = -188.4, axis = Axis(title = "Bias", offset = 0D, grid = true)).
        encodeX(field = "y", value = 2.67, axis = Axis(title = "Weight", offset = 0D, grid = true))


      ,
      Layer().
        mark(Line).
        encodeX("x", Quant).
        encodeY("y", Quant)
    )
    .show
}
