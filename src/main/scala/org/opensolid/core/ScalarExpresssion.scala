////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  OpenSolid is a generic library for the representation and manipulation    //
//  of geometric objects such as points, curves, surfaces, and volumes.       //
//                                                                            //
//  Copyright 2007-2015 by Ian Mackenzie                                      //
//  ian.e.mackenzie@gmail.com                                                 //
//                                                                            //
//  This Source Code Form is subject to the terms of the Mozilla Public       //
//  License, v. 2.0. If a copy of the MPL was not distributed with this file, //
//  you can obtain one at http://mozilla.org/MPL/2.0/.                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

package org.opensolid.core

sealed abstract class ScalarExpression[P] {
  import ScalarExpression._

  def derivative(parameter: P): ScalarExpression[P]

  def unary_- : ScalarExpression[P] =
    Negation(this)

  final def negated: ScalarExpression[P] = -this

  final def +(that: ScalarExpression[P]): ScalarExpression[P] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue + secondValue)
    case (expression, Constant(0)) => expression
    case (Constant(0), expression) => expression
    case (first, second) if (first == second) => Constant(2) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def +(value: Double): ScalarExpression[P] =
    this + Constant[P](value)

  final def plus(that: ScalarExpression[P]): ScalarExpression[P] =
    this + that

  final def plus(value: Double): ScalarExpression[P] =
    this + Constant[P](value)

  final def -(that: ScalarExpression[P]): ScalarExpression[P] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue - secondValue)
    case (expression, Constant(0)) => expression
    case (Constant(0), expression) => -expression
    case (first, second) if (first == second) => Constant(0)
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def -(value: Double): ScalarExpression[P] =
    this - Constant[P](value)

  final def minus(that: ScalarExpression[P]): ScalarExpression[P] =
    this - that

  final def minus(value: Double): ScalarExpression[P] =
    this - Constant[P](value)

  final def *(that: ScalarExpression[P]): ScalarExpression[P] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue * secondValue)
    case (_, Constant(0)) => Constant(0)
    case (Constant(0), _) => Constant(0)
    case (expression, Constant(1)) => expression
    case (Constant(1), expression) => expression
    case (expression, Constant(-1)) => -expression
    case (Constant(-1), expression) => -expression
    case (first, second) if (first == second) => first.squared
    case (Quotient(a, b), Quotient(c, d)) => (a * c) / (b * d)
    case _ => Product(this, that)
  }

  final def *(value: Double): ScalarExpression[P] =
    this * Constant[P](value)

  final def times(that: ScalarExpression[P]): ScalarExpression[P] =
    this * that

  final def times(value: Double): ScalarExpression[P] =
    this * Constant[P](value)

  final def *(that: VectorExpression2d[P]): VectorExpression2d[P] =
    that * this

  final def times(that: VectorExpression2d[P]): VectorExpression2d[P] =
    that * this

  final def *(vector: Vector2d): VectorExpression2d[P] =
    VectorExpression2d.Constant[P](vector) * this

  final def times(vector: Vector2d): VectorExpression2d[P] =
    VectorExpression2d.Constant[P](vector) * this

  final def *(that: VectorExpression3d[P]): VectorExpression3d[P] =
    that * this

  final def times(that: VectorExpression3d[P]): VectorExpression3d[P] =
    that * this

  final def *(vector: Vector3d): VectorExpression3d[P] =
    VectorExpression3d.Constant[P](vector) * this

  final def times(vector: Vector3d): VectorExpression3d[P] =
    VectorExpression3d.Constant[P](vector) * this

  final def /(that: ScalarExpression[P]): ScalarExpression[P] = (this, that) match {
    case (_, Constant(0)) => throw new ArithmeticException("Division by zero")
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue / secondValue)
    case (Constant(0), _) => Constant(0)
    case (expression, Constant(1)) => expression
    case (expression, Constant(-1)) => -expression
    case (expression, Constant(value)) => Constant(1 / value) * expression
    case (expression, Quotient(numerator, denominator)) => expression * denominator / numerator
    case _ => Quotient(this, that)
  }

  final def /(value: Double): ScalarExpression[P] =
    this / Constant[P](value)

  final def dividedBy(that: ScalarExpression[P]): ScalarExpression[P] =
    this / that

  final def dividedBy(value: Double): ScalarExpression[P] =
    this / Constant[P](value)

  def squared: ScalarExpression[P] =
    Square(this)

  def sqrt: ScalarExpression[P] =
    SquareRoot(this)

  def sin: ScalarExpression[P] =
    Sine(this)

  def cos: ScalarExpression[P] =
    Cosine(this)

  def tan: ScalarExpression[P] =
    Tangent(this)

  def asin: ScalarExpression[P] =
    Arcsine(this)

  def acos: ScalarExpression[P] =
    Arccosine(this)

  def atan: ScalarExpression[P] =
    Arctangent(this)

  final def toCurveFunction(implicit evidence: P =:= CurveParameter): CurveFunction1d = {
    val compiler = new ExpressionCompiler(1)
    val resultIndex = compiler.evaluate(this)
    val arrayOperations = compiler.arrayOperations.toArray
    val arraySize = compiler.arraySize

    new CurveFunction1d {
      override def apply(parameterValue: Double): Double = {
        val array = Array.ofDim[Double](arraySize)
        array(0) = parameterValue
        for (operation <- arrayOperations) {
          operation.execute(array)
        }
        array(resultIndex)
      }

      override def apply(parameterBounds: Interval): Interval = {
        val array = Array.ofDim[Interval](arraySize)
        array(0) = parameterBounds
        for (operation <- arrayOperations) {
          operation.execute(array)
        }
        array(resultIndex)
      }
    }
  }

  final def toSurfaceFunction(implicit evidence: P =:= SurfaceParameter): SurfaceFunction1d = {
    val compiler = new ExpressionCompiler(2)
    val resultIndex = compiler.evaluate(this)
    val arrayOperations = compiler.arrayOperations.toArray
    val arraySize = compiler.arraySize

    new SurfaceFunction1d {
      override def apply(parameterValue: Point2d): Double = {
        val array = Array.ofDim[Double](arraySize)
        array(0) = parameterValue.x
        array(1) = parameterValue.y
        for (operation <- arrayOperations) {
          operation.execute(array)
        }
        array(resultIndex)
      }

      override def apply(parameterBounds: Bounds2d): Interval = {
        val array = Array.ofDim[Interval](arraySize)
        array(0) = parameterBounds.x
        array(1) = parameterBounds.y
        for (operation <- arrayOperations) {
          operation.execute(array)
        }
        array(resultIndex)
      }
    }
  }
}

object ScalarExpression {
  abstract class Parameter[P <: Parameter[P]] extends ScalarExpression[P] {
    def index: Int

    override def derivative(parameter: P): ScalarExpression[P] =
      if (parameter eq this) Constant(1) else Constant(0)
  }

  case class Constant[P](val value: Double) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      Constant(0)

    override def unary_- : ScalarExpression[P] =
      Constant(-value)

    override def squared: ScalarExpression[P] =
      Constant(value * value)
  }

  case class Negation[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      -expression.derivative(parameter)

    override def unary_- : ScalarExpression[P] =
      expression

    override def squared: ScalarExpression[P] =
      expression.squared
  }

  case class Sum[P](
    firstExpression: ScalarExpression[P],
    secondExpression: ScalarExpression[P]
  ) extends ScalarExpression[P] {

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter) + secondExpression.derivative(parameter)
  }

  case class Difference[P](
    firstExpression: ScalarExpression[P],
    secondExpression: ScalarExpression[P]
  ) extends ScalarExpression[P] {

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter) - secondExpression.derivative(parameter)

    override def unary_- : ScalarExpression[P] =
      Difference[P](secondExpression, firstExpression)
  }

  case class Product[P](
    firstExpression: ScalarExpression[P],
    secondExpression: ScalarExpression[P]
  ) extends ScalarExpression[P] {

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter) * secondExpression +
      firstExpression * secondExpression.derivative(parameter)
  }

  case class Quotient[P](
    firstExpression: ScalarExpression[P],
    secondExpression: ScalarExpression[P]
  ) extends ScalarExpression[P] {

    override def derivative(parameter: P): ScalarExpression[P] =
      (
        firstExpression.derivative(parameter) * secondExpression -
        firstExpression * secondExpression.derivative(parameter)
      ) / secondExpression.squared
  }

  case class Square[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      2 * expression * expression.derivative(parameter)
  }

  case class VectorXComponent2d[P](expression: VectorExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).x
  }

  case class VectorYComponent2d[P](expression: VectorExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).y
  }

  case class VectorXComponent3d[P](expression: VectorExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).x
  }

  case class VectorYComponent3d[P](expression: VectorExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).y
  }

  case class VectorZComponent3d[P](expression: VectorExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).z
  }

  case class PointXComponent2d[P](expression: PointExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).x
  }

  case class PointYComponent2d[P](expression: PointExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).y
  }

  case class PointXComponent3d[P](expression: PointExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).x
  }

  case class PointYComponent3d[P](expression: PointExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).y
  }

  case class PointZComponent3d[P](expression: PointExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).z
  }

  case class DotProduct2d[P](
    firstExpression: VectorExpression2d[P],
    secondExpression: VectorExpression2d[P]
  ) extends ScalarExpression[P] {

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter).dot(secondExpression) +
      firstExpression.dot(secondExpression.derivative(parameter))
  }

  case class DotProduct3d[P](
    firstExpression: VectorExpression3d[P],
    secondExpression: VectorExpression3d[P]
  ) extends ScalarExpression[P] {

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter).dot(secondExpression) +
      firstExpression.dot(secondExpression.derivative(parameter))
  }

  case class CrossProduct2d[P](
    firstExpression: VectorExpression2d[P],
    secondExpression: VectorExpression2d[P]
  ) extends ScalarExpression[P] {

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter).cross(secondExpression) +
      firstExpression.cross(secondExpression.derivative(parameter))
  }

  case class SquaredLength2d[P](expression: VectorExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      2 * expression.dot(expression.derivative(parameter))
  }

  case class SquaredLength3d[P](expression: VectorExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      2 * expression.dot(expression.derivative(parameter))
  }

  case class SquareRoot[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter) / (2 * this)
  }

  case class Sine[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.cos * expression.derivative(parameter)
  }

  case class Cosine[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      -expression.sin * expression.derivative(parameter)
  }

  case class Tangent[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter) / expression.cos.squared
  }

  case class Arcsine[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter) / (1 - expression.squared).sqrt
  }

  case class Arccosine[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      -expression.derivative(parameter) / (1 - expression.squared).sqrt
  }

  case class Arctangent[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter) / (1 + expression.squared)
  }
}
