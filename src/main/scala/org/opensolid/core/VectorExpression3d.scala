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

import scala.math

sealed abstract class VectorExpression3d[P] {
  import VectorExpression3d._

  def derivative(parameter: P): VectorExpression3d[P]

  def condition: ScalarExpression[P]

  def unary_- : VectorExpression3d[P] =
    Negation(this)

  final def negated: VectorExpression3d[P] =
    -this

  final def +(that: VectorExpression3d[P]): VectorExpression3d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) => Constant(firstVector + secondVector)
    case (expression, Constant(Vector3d.Zero)) => expression
    case (Constant(Vector3d.Zero), expression) => expression
    case (first, second) if (first == second) => ScalarExpression.Constant(2) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def +(vector: Vector3d): VectorExpression3d[P] =
    this + Constant[P](vector)

  final def plus(that: VectorExpression3d[P]): VectorExpression3d[P] =
    this + that

  final def plus(vector: Vector3d): VectorExpression3d[P] =
    this + vector

  final def -(that: VectorExpression3d[P]): VectorExpression3d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) => Constant(firstVector - secondVector)
    case (expression, Constant(Vector3d.Zero)) => expression
    case (Constant(Vector3d.Zero), expression) => -expression
    case (first, second) if (first == second) => Constant(Vector3d.Zero)
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def -(vector: Vector3d): VectorExpression3d[P] =
    this - Constant[P](vector)

  final def minus(that: VectorExpression3d[P]): VectorExpression3d[P] =
    this - that

  final def minus(vector: Vector3d): VectorExpression3d[P] =
    this - vector

  final def *(scalarExpression: ScalarExpression[P]): VectorExpression3d[P] =
    (this, scalarExpression) match {
      case (Constant(vector), ScalarExpression.Constant(value)) => Constant(vector * value)
      case (Constant(Vector3d.Zero), _) => Constant(Vector3d.Zero)
      case (_, ScalarExpression.Constant(0)) => Constant(Vector3d.Zero)
      case (expression, ScalarExpression.Constant(1)) => expression
      case (expression, ScalarExpression.Constant(-1)) => -expression
      case (Quotient(a, b), ScalarExpression.Quotient(c, d)) => (a * c) / (b * d)
      case _ => VectorExpression3d.Product(scalarExpression, this)
    }

  final def *(value: Double): VectorExpression3d[P] =
    this * ScalarExpression.Constant[P](value)

  final def times(scalarExpression: ScalarExpression[P]): VectorExpression3d[P] =
    this * scalarExpression

  final def times(value: Double): VectorExpression3d[P] =
    this * value

  final def /(scalarExpression: ScalarExpression[P]): VectorExpression3d[P] =
    (this, scalarExpression) match {
      case (_, ScalarExpression.Constant(0)) => throw new ArithmeticException("Division by zero")
      case (Constant(vector), ScalarExpression.Constant(divisor)) => Constant(vector / divisor)
      case (Constant(Vector3d.Zero), _) => Constant(Vector3d.Zero)
      case (expression, ScalarExpression.Constant(1)) => expression
      case (expression, ScalarExpression.Constant(-1)) => -expression
      case (expression, ScalarExpression.Constant(value)) =>
        ScalarExpression.Constant(1 / value) * expression
      case (expression, ScalarExpression.Quotient(numerator, denominator)) =>
        expression * (denominator / numerator)
      case _ => Quotient(this, scalarExpression)
    }

  final def /(value: Double): VectorExpression3d[P] =
    this / ScalarExpression.Constant[P](value)

  final def dividedBy(scalarExpression: ScalarExpression[P]): VectorExpression3d[P] =
    this / scalarExpression

  final def dividedBy(value: Double): VectorExpression3d[P] =
    this / value

  def squaredLength: ScalarExpression[P] =
    ScalarExpression.SquaredLength3d(this)

  def length: ScalarExpression[P] =
    ScalarExpression.sqrt(squaredLength)

  final def dot(that: VectorExpression3d[P]): ScalarExpression[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) =>
      ScalarExpression.Constant(firstVector.dot(secondVector))
    case (Constant(Vector3d.Zero), _) => ScalarExpression.Constant(0)
    case (_, Constant(Vector3d.Zero)) => ScalarExpression.Constant(0)
    case (Constant(Vector3d(1, 0, 0)), expression) => expression.x
    case (Constant(Vector3d(-1, 0, 0)), expression) => -expression.x
    case (Constant(Vector3d(0, 1, 0)), expression) => expression.y
    case (Constant(Vector3d(0, -1, 0)), expression) => -expression.y
    case (Constant(Vector3d(0, 0, 1)), expression) => expression.z
    case (Constant(Vector3d(0, 0, -1)), expression) => -expression.z
    case (expression, Constant(Vector3d(1, 0, 0))) => expression.x
    case (expression, Constant(Vector3d(-1, 0, 0))) => -expression.x
    case (expression, Constant(Vector3d(0, 1, 0))) => expression.y
    case (expression, Constant(Vector3d(0, -1, 0))) => -expression.y
    case (expression, Constant(Vector3d(0, 0, 1))) => expression.z
    case (expression, Constant(Vector3d(0, 0, -1))) => -expression.z
    case (first, second) if (first == second) => first.squaredLength
    case (first, second) if (first == -second) => -first.squaredLength
    case _ => ScalarExpression.DotProduct3d(this, that)
  }

  final def cross(that: VectorExpression3d[P]): VectorExpression3d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) =>
      Constant(firstVector.cross(secondVector))
    case (Constant(Vector3d.Zero), _) => Constant[P](Vector3d.Zero)
    case (_, Constant(Vector3d.Zero)) => Constant[P](Vector3d.Zero)
    case (first, second) if (first == second) => Constant[P](Vector3d.Zero)
    case (first, second) if (first == -second) => Constant[P](Vector3d.Zero)
    case _ => CrossProduct(this, that)
  }

  def x: ScalarExpression[P] =
    ScalarExpression.VectorXComponent3d(this)

  def y: ScalarExpression[P] =
    ScalarExpression.VectorYComponent3d(this)

  def z: ScalarExpression[P] =
    ScalarExpression.VectorZComponent3d(this)
}

object VectorExpression3d {
  def fromComponents[P](
    xExpression: ScalarExpression[P],
    yExpression: ScalarExpression[P],
    zExpression: ScalarExpression[P]
  ): VectorExpression3d[P] = (xExpression, yExpression, zExpression) match {
    case (
      ScalarExpression.Constant(xValue),
      ScalarExpression.Constant(yValue),
      ScalarExpression.Constant(zValue)
    ) => Constant(Vector3d(xValue, yValue, zValue))
    case _ => FromComponents(xExpression, yExpression, zExpression)
  }

  def compileCurve[P <: CurveParameter](expression: VectorExpression3d[P]): CompiledCurve = {
    val compiler = new ExpressionCompiler(1)
    val (xIndex, yIndex, zIndex) = compiler.evaluate(expression)
    new CompiledCurve(
      compiler.arrayOperations.toArray,
      compiler.arraySize,
      xIndex,
      yIndex,
      zIndex
    )
  }

  def compileSurface[P <: SurfaceParameter](expression: VectorExpression3d[P]): CompiledSurface = {
    val compiler = new ExpressionCompiler(2)
    val (xIndex, yIndex, zIndex) = compiler.evaluate(expression)
    new CompiledSurface(
      compiler.arrayOperations.toArray,
      compiler.arraySize,
      xIndex,
      yIndex,
      zIndex
    )
  }

  class CompiledCurve private[VectorExpression3d] (
    arrayOperations: Array[ExpressionCompiler.ArrayOperation],
    arraySize: Int,
    xIndex: Int,
    yIndex: Int,
    zIndex: Int
  ) {
    def evaluate(parameterValue: Double): Vector3d = {
      val array = Array.ofDim[Double](arraySize)
      array(0) = parameterValue
      for { operation <- arrayOperations } operation.execute(array)
      Vector3d(array(xIndex), array(yIndex), array(zIndex))
    }

    def evaluate(parameterBounds: Interval): VectorBounds3d = {
      val array = Array.ofDim[Interval](arraySize)
      array(0) = parameterBounds
      for { operation <- arrayOperations } operation.execute(array)
      VectorBounds3d(array(xIndex), array(yIndex), array(zIndex))
    }
  }

  class CompiledSurface private[VectorExpression3d] (
    arrayOperations: Array[ExpressionCompiler.ArrayOperation],
    arraySize: Int,
    xIndex: Int,
    yIndex: Int,
    zIndex: Int
  ) {
    def evaluate(parameterValue: Point2d): Vector3d = {
      val array = Array.ofDim[Double](arraySize)
      array(0) = parameterValue.x
      array(1) = parameterValue.y
      for { operation <- arrayOperations } operation.execute(array)
      Vector3d(array(xIndex), array(yIndex), array(zIndex))
    }

    def evaluate(parameterBounds: Bounds2d): VectorBounds3d = {
      val array = Array.ofDim[Interval](arraySize)
      array(0) = parameterBounds.x
      array(1) = parameterBounds.y
      for { operation <- arrayOperations } operation.execute(array)
      VectorBounds3d(array(xIndex), array(yIndex), array(zIndex))
    }
  }

  case class Constant[P](val vector: Vector3d) extends VectorExpression3d[P] {
    override def derivative(parameter: P): VectorExpression3d[P] =
      Constant(Vector3d.Zero)

    override def condition: ScalarExpression[P] =
      ScalarExpression.Constant(1)

    override def unary_- : VectorExpression3d[P] =
      Constant(-vector)

    override def squaredLength: ScalarExpression[P] =
      ScalarExpression.Constant(vector.squaredLength)

    override def length: ScalarExpression[P] =
      ScalarExpression.Constant(vector.length)

    override def x: ScalarExpression[P] =
      ScalarExpression.Constant(vector.x)

    override def y: ScalarExpression[P] =
      ScalarExpression.Constant(vector.y)

    override def z: ScalarExpression[P] =
      ScalarExpression.Constant(vector.z)
  }

  case class FromComponents[P](
    override val x: ScalarExpression[P],
    override val y: ScalarExpression[P],
    override val z: ScalarExpression[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      VectorExpression3d.fromComponents(
        x.derivative(parameter),
        y.derivative(parameter),
        z.derivative(parameter)
      )

    override def condition: ScalarExpression[P] =
      x.condition * y.condition * z.condition

    override def unary_- : VectorExpression3d[P] =
      VectorExpression3d.fromComponents(-x, -y, -z)

    override def squaredLength: ScalarExpression[P] =
      x.squared + y.squared + z.squared
  }

  case class Negation[P](expression: VectorExpression3d[P]) extends VectorExpression3d[P] {
    override def derivative(parameter: P): VectorExpression3d[P] =
      -expression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      expression.condition

    override def squaredLength: ScalarExpression[P] =
      expression.squaredLength

    override def length: ScalarExpression[P] =
      expression.length

    override def x: ScalarExpression[P] =
      -expression.x

    override def y: ScalarExpression[P] =
      -expression.y

    override def z: ScalarExpression[P] =
      -expression.z
  }

  case class Sum[P](
    firstExpression: VectorExpression3d[P],
    secondExpression: VectorExpression3d[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      firstExpression.derivative(parameter) + secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression.x + secondExpression.x

    override def y: ScalarExpression[P] =
      firstExpression.y + secondExpression.y

    override def z: ScalarExpression[P] =
      firstExpression.z + secondExpression.z
  }

  case class Difference[P](
    firstExpression: VectorExpression3d[P],
    secondExpression: VectorExpression3d[P]
  ) extends VectorExpression3d[P] {

    override def unary_- : VectorExpression3d[P] =
      Difference[P](secondExpression, firstExpression)

    override def derivative(parameter: P): VectorExpression3d[P] =
      firstExpression.derivative(parameter) - secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression.x - secondExpression.x

    override def y: ScalarExpression[P] =
      firstExpression.y - secondExpression.y

    override def z: ScalarExpression[P] =
      firstExpression.z - secondExpression.z
  }

  case class PointDifference[P](
    firstExpression: PointExpression3d[P],
    secondExpression: PointExpression3d[P]
  ) extends VectorExpression3d[P] {

    override def unary_- : VectorExpression3d[P] =
      PointDifference[P](secondExpression, firstExpression)

    override def derivative(parameter: P): VectorExpression3d[P] =
      firstExpression.derivative(parameter) - secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression.x - secondExpression.x

    override def y: ScalarExpression[P] =
      firstExpression.y - secondExpression.y

    override def z: ScalarExpression[P] =
      firstExpression.z - secondExpression.z
  }

  case class Product[P](
    scalarExpression: ScalarExpression[P],
    vectorExpression: VectorExpression3d[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      scalarExpression.derivative(parameter) * vectorExpression +
      scalarExpression * vectorExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      scalarExpression.condition * vectorExpression.condition

    override def x: ScalarExpression[P] =
      scalarExpression * vectorExpression.x

    override def y: ScalarExpression[P] =
      scalarExpression * vectorExpression.y

    override def z: ScalarExpression[P] =
      scalarExpression * vectorExpression.z
  }

  case class Quotient[P](
    vectorExpression: VectorExpression3d[P],
    scalarExpression: ScalarExpression[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      (
        vectorExpression.derivative(parameter) * scalarExpression -
        vectorExpression * scalarExpression.derivative(parameter)
      ) / scalarExpression.squared

    override def condition: ScalarExpression[P] =
      vectorExpression.condition * scalarExpression

    override def x: ScalarExpression[P] =
      vectorExpression.x / scalarExpression

    override def y: ScalarExpression[P] =
      vectorExpression.y / scalarExpression

    override def z: ScalarExpression[P] =
      vectorExpression.z / scalarExpression
  }

  case class CrossProduct[P](
    firstExpression: VectorExpression3d[P],
    secondExpression: VectorExpression3d[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      firstExpression.derivative(parameter).cross(secondExpression) +
      firstExpression.cross(secondExpression.derivative(parameter))

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression.y * secondExpression.z - firstExpression.z * secondExpression.y

    override def y: ScalarExpression[P] =
      firstExpression.z * secondExpression.x - firstExpression.x * secondExpression.z

    override def z: ScalarExpression[P] =
      firstExpression.x * secondExpression.y - firstExpression.y * secondExpression.x
  }
}
