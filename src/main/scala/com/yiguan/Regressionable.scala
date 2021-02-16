package com.yiguan

import java.lang.Math.sqrt

import com.yiguan.Laziness.repeat

trait Regressionable {

  case class It(factors: Factors, learningRates: LearningRates)

  type TrainingData = Vector[Double]
  type Factors = Vector[Double]
  type LearningRates = Vector[Double]

  def initialFactors: Factors
  def initialLearningRates: LearningRates
  def derivativeFunctions: Vector[Factors => TrainingData => Double]
  def trainingDataSet: List[TrainingData]

  lazy val zeros = derivativeFunctions.map(_ => 0D)

  private def gradient: It => It = {
    case It(factors, learningRates) =>
      val grads = sumPartialDerivative(factors)
      val nextLearningRates = nextLearningRate(grads)(learningRates)
      val deltas = nextLearningRates zip grads map { case (lr, g) => 1 / sqrt(lr) * g }
      val nextFactors = factors zip deltas map { case (factor, delta) => factor - delta }
      It(nextFactors, nextLearningRates)
  }

  private def sumPartialDerivative(factors: Factors): Vector[Double] = trainingDataSet.foldLeft(zeros) { (sumVector, trainingData) =>
    sumVector.zip(derivativeFunctions).map {
      case (sum, f) => sum - f(factors)(trainingData)
    }
  }

  private def nextLearningRate(sumDerivatives: Vector[Double])(learningRates: LearningRates): LearningRates =
    sumDerivatives zip learningRates map {
      case (derivative, lr) => lr + Math.pow(derivative, 2)
    }

  lazy val its: Stream[Factors] = repeat(gradient)(It(initialFactors, initialLearningRates)) map (_.factors)
}
