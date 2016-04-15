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

  def unary_- : VectorExpression3d[P] =
    Negation(this)

  final def negated: VectorExpression3d[P] =
    -this

  final def +(that: VectorExpression3d[P]): VectorExpression3d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) => Constant(firstVector + secondVector)
    case (expression, Constant(Vector3d.Zero)) => expression
    case (Constant(Vector3d.Zero), expression) => expression
    case (first, second) if (first == second) => Expression1d.Constant(2) * first
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

  final def *(scalarExpression: Expression1d[P]): VectorExpression3d[P] =
    (this, scalarExpression) match {
      case (Constant(vector), Expression1d.Constant(value)) => Constant(vector * value)
      case (Constant(Vector3d.Zero), _) => Constant(Vector3d.Zero)
      case (_, Expression1d.Constant(0)) => Constant(Vector3d.Zero)
      case (expression, Expression1d.Constant(1)) => expression
      case (expression, Expression1d.Constant(-1)) => -expression
      case (Quotient(a, b), Expression1d.Quotient(c, d)) => (a * c) / (b * d)
      case _ => VectorExpression3d.Product(scalarExpression, this)
    }

  final def *(value: Double): VectorExpression3d[P] =
    this * Expression1d.Constant[P](value)

  final def times(scalarExpression: Expression1d[P]): VectorExpression3d[P] =
    this * scalarExpression

  final def times(value: Double): VectorExpression3d[P] =
    this * value

  final def /(scalarExpression: Expression1d[P]): VectorExpression3d[P] =
    (this, scalarExpression) match {
      case (_, Expression1d.Constant(0)) => throw new ArithmeticException("Division by zero")
      case (Constant(vector), Expression1d.Constant(divisor)) => Constant(vector / divisor)
      case (Constant(Vector3d.Zero), _) => Constant(Vector3d.Zero)
      case (expression, Expression1d.Constant(1)) => expression
      case (expression, Expression1d.Constant(-1)) => -expression
      case (expression, Expression1d.Constant(value)) =>
        Expression1d.Constant(1 / value) * expression
      case (expression, Expression1d.Quotient(numerator, denominator)) =>
        expression * (denominator / numerator)
      case _ => Quotient(this, scalarExpression)
    }

  final def /(value: Double): VectorExpression3d[P] =
    this / Expression1d.Constant[P](value)

  final def dividedBy(scalarExpression: Expression1d[P]): VectorExpression3d[P] =
    this / scalarExpression

  final def dividedBy(value: Double): VectorExpression3d[P] =
    this / value

  def squaredLength: Expression1d[P] =
    Expression1d.SquaredLength3d(this)

  def length: Expression1d[P] =
    squaredLength.sqrt

  final def dot(that: VectorExpression3d[P]): Expression1d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) =>
      Expression1d.Constant(firstVector.dot(secondVector))
    case (Constant(Vector3d.Zero), _) => Expression1d.Constant(0)
    case (_, Constant(Vector3d.Zero)) => Expression1d.Constant(0)
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
    case _ => Expression1d.DotProduct3d(this, that)
  }

  final def dot(vector: Vector3d): Expression1d[P] =
    dot(Constant[P](vector))

  final def cross(that: VectorExpression3d[P]): VectorExpression3d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) =>
      Constant(firstVector.cross(secondVector))
    case (Constant(Vector3d.Zero), _) => Constant[P](Vector3d.Zero)
    case (_, Constant(Vector3d.Zero)) => Constant[P](Vector3d.Zero)
    case (first, second) if (first == second) => Constant[P](Vector3d.Zero)
    case (first, second) if (first == -second) => Constant[P](Vector3d.Zero)
    case _ => CrossProduct(this, that)
  }

  final def cross(vector: Vector3d): VectorExpression3d[P] =
    cross(Constant[P](vector))

  final def componentIn(direction: Direction3d): Expression1d[P] =
    dot(direction.vector)

  def x: Expression1d[P] =
    Expression1d.VectorXComponent3d(this)

  def y: Expression1d[P] =
    Expression1d.VectorYComponent3d(this)

  def z: Expression1d[P] =
    Expression1d.VectorZComponent3d(this)
}

object VectorExpression3d {
  def fromComponents[P](
    xExpression: Expression1d[P],
    yExpression: Expression1d[P],
    zExpression: Expression1d[P]
  ): VectorExpression3d[P] = (xExpression, yExpression, zExpression) match {
    case (
      Expression1d.Constant(xValue),
      Expression1d.Constant(yValue),
      Expression1d.Constant(zValue)
    ) => Constant(Vector3d(xValue, yValue, zValue))
    case _ => FromComponents(xExpression, yExpression, zExpression)
  }

  case class Constant[P](val vector: Vector3d) extends VectorExpression3d[P] {
    override def derivative(parameter: P): VectorExpression3d[P] =
      Constant(Vector3d.Zero)

    override def unary_- : VectorExpression3d[P] =
      Constant(-vector)

    override def squaredLength: Expression1d[P] =
      Expression1d.Constant(vector.squaredLength)

    override def length: Expression1d[P] =
      Expression1d.Constant(vector.length)

    override def x: Expression1d[P] =
      Expression1d.Constant(vector.x)

    override def y: Expression1d[P] =
      Expression1d.Constant(vector.y)

    override def z: Expression1d[P] =
      Expression1d.Constant(vector.z)
  }

  case class FromComponents[P](
    override val x: Expression1d[P],
    override val y: Expression1d[P],
    override val z: Expression1d[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      VectorExpression3d.fromComponents(
        x.derivative(parameter),
        y.derivative(parameter),
        z.derivative(parameter)
      )

    override def unary_- : VectorExpression3d[P] =
      VectorExpression3d.fromComponents(-x, -y, -z)

    override def squaredLength: Expression1d[P] =
      x.squared + y.squared + z.squared
  }

  case class Negation[P](expression: VectorExpression3d[P]) extends VectorExpression3d[P] {
    override def derivative(parameter: P): VectorExpression3d[P] =
      -expression.derivative(parameter)

    override def squaredLength: Expression1d[P] =
      expression.squaredLength

    override def length: Expression1d[P] =
      expression.length

    override def x: Expression1d[P] =
      -expression.x

    override def y: Expression1d[P] =
      -expression.y

    override def z: Expression1d[P] =
      -expression.z
  }

  case class Sum[P](
    firstExpression: VectorExpression3d[P],
    secondExpression: VectorExpression3d[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      firstExpression.derivative(parameter) + secondExpression.derivative(parameter)

    override def x: Expression1d[P] =
      firstExpression.x + secondExpression.x

    override def y: Expression1d[P] =
      firstExpression.y + secondExpression.y

    override def z: Expression1d[P] =
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

    override def x: Expression1d[P] =
      firstExpression.x - secondExpression.x

    override def y: Expression1d[P] =
      firstExpression.y - secondExpression.y

    override def z: Expression1d[P] =
      firstExpression.z - secondExpression.z
  }

  case class Displacement[P](
    firstExpression: PointExpression3d[P],
    secondExpression: PointExpression3d[P]
  ) extends VectorExpression3d[P] {

    override def unary_- : VectorExpression3d[P] =
      Displacement[P](secondExpression, firstExpression)

    override def derivative(parameter: P): VectorExpression3d[P] =
      secondExpression.derivative(parameter) - firstExpression.derivative(parameter)

    override def x: Expression1d[P] =
      secondExpression.x - firstExpression.x

    override def y: Expression1d[P] =
      secondExpression.y - firstExpression.y

    override def z: Expression1d[P] =
      secondExpression.z - firstExpression.z
  }

  case class Product[P](
    scalarExpression: Expression1d[P],
    vectorExpression: VectorExpression3d[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      scalarExpression.derivative(parameter) * vectorExpression +
      scalarExpression * vectorExpression.derivative(parameter)

    override def x: Expression1d[P] =
      scalarExpression * vectorExpression.x

    override def y: Expression1d[P] =
      scalarExpression * vectorExpression.y

    override def z: Expression1d[P] =
      scalarExpression * vectorExpression.z
  }

  case class Quotient[P](
    vectorExpression: VectorExpression3d[P],
    scalarExpression: Expression1d[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      ( vectorExpression.derivative(parameter) * scalarExpression -
        vectorExpression * scalarExpression.derivative(parameter) ) /
      scalarExpression.squared

    override def x: Expression1d[P] =
      vectorExpression.x / scalarExpression

    override def y: Expression1d[P] =
      vectorExpression.y / scalarExpression

    override def z: Expression1d[P] =
      vectorExpression.z / scalarExpression
  }

  case class CrossProduct[P](
    firstExpression: VectorExpression3d[P],
    secondExpression: VectorExpression3d[P]
  ) extends VectorExpression3d[P] {

    override def derivative(parameter: P): VectorExpression3d[P] =
      firstExpression.derivative(parameter).cross(secondExpression) +
      firstExpression.cross(secondExpression.derivative(parameter))

    override def x: Expression1d[P] =
      firstExpression.y * secondExpression.z - firstExpression.z * secondExpression.y

    override def y: Expression1d[P] =
      firstExpression.z * secondExpression.x - firstExpression.x * secondExpression.z

    override def z: Expression1d[P] =
      firstExpression.x * secondExpression.y - firstExpression.y * secondExpression.x
  }
}
