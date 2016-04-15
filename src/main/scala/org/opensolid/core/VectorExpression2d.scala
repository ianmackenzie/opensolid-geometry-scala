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

sealed abstract class VectorExpression2d[P] {
  import VectorExpression2d._

  def derivative(parameter: P): VectorExpression2d[P]

  def unary_- : VectorExpression2d[P] =
    Negation(this)

  final def negated: VectorExpression2d[P] =
    -this

  final def +(that: VectorExpression2d[P]): VectorExpression2d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) => Constant(firstVector + secondVector)
    case (expression, Constant(Vector2d.Zero)) => expression
    case (Constant(Vector2d.Zero), expression) => expression
    case (first, second) if (first == second) => Expression1d.Constant(2) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def +(vector: Vector2d): VectorExpression2d[P] =
    this + Constant[P](vector)

  final def plus(that: VectorExpression2d[P]): VectorExpression2d[P] =
    this + that

  final def plus(vector: Vector2d): VectorExpression2d[P] =
    this + vector

  final def -(that: VectorExpression2d[P]): VectorExpression2d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) => Constant(firstVector - secondVector)
    case (expression, Constant(Vector2d.Zero)) => expression
    case (Constant(Vector2d.Zero), expression) => -expression
    case (first, second) if (first == second) => Constant(Vector2d.Zero)
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def -(vector: Vector2d): VectorExpression2d[P] =
    this - Constant[P](vector)

  final def minus(that: VectorExpression2d[P]): VectorExpression2d[P] =
    this - that

  final def minus(vector: Vector2d): VectorExpression2d[P] =
    this - vector

  final def *(scalarExpression: Expression1d[P]): VectorExpression2d[P] =
    (this, scalarExpression) match {
      case (Constant(vector), Expression1d.Constant(value)) => Constant(vector * value)
      case (Constant(Vector2d.Zero), _) => Constant(Vector2d.Zero)
      case (_, Expression1d.Constant(0)) => Constant(Vector2d.Zero)
      case (expression, Expression1d.Constant(1)) => expression
      case (expression, Expression1d.Constant(-1)) => -expression
      case (Quotient(a, b), Expression1d.Quotient(c, d)) => (a * c) / (b * d)
      case _ => VectorExpression2d.Product(scalarExpression, this)
    }

  final def *(value: Double): VectorExpression2d[P] =
    this * Expression1d.Constant[P](value)

  final def times(scalarExpression: Expression1d[P]): VectorExpression2d[P] =
    this * scalarExpression

  final def times(value: Double): VectorExpression2d[P] =
    this * value

  final def /(scalarExpression: Expression1d[P]): VectorExpression2d[P] =
    (this, scalarExpression) match {
      case (_, Expression1d.Constant(0)) => throw new ArithmeticException("Division by zero")
      case (Constant(vector), Expression1d.Constant(divisor)) => Constant(vector / divisor)
      case (Constant(Vector2d.Zero), _) => Constant(Vector2d.Zero)
      case (expression, Expression1d.Constant(1)) => expression
      case (expression, Expression1d.Constant(-1)) => -expression
      case (expression, Expression1d.Constant(value)) =>
        Expression1d.Constant(1 / value) * expression
      case (expression, Expression1d.Quotient(numerator, denominator)) =>
        expression * (denominator / numerator)
      case _ => Quotient(this, scalarExpression)
    }

  final def /(value: Double): VectorExpression2d[P] =
    this / Expression1d.Constant[P](value)

  final def dividedBy(scalarExpression: Expression1d[P]): VectorExpression2d[P] =
    this / scalarExpression

  final def dividedBy(value: Double): VectorExpression2d[P] =
    this / value

  def squaredLength: Expression1d[P] =
    Expression1d.SquaredLength2d(this)

  def length: Expression1d[P] =
    squaredLength.sqrt

  final def dot(that: VectorExpression2d[P]): Expression1d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) =>
      Expression1d.Constant(firstVector.dot(secondVector))
    case (Constant(Vector2d.Zero), _) => Expression1d.Constant(0)
    case (_, Constant(Vector2d.Zero)) => Expression1d.Constant(0)
    case (Constant(Vector2d(1, 0)), expression) => expression.x
    case (Constant(Vector2d(-1, 0)), expression) => -expression.x
    case (Constant(Vector2d(0, 1)), expression) => expression.y
    case (Constant(Vector2d(0, -1)), expression) => -expression.y
    case (expression, Constant(Vector2d(1, 0))) => expression.x
    case (expression, Constant(Vector2d(-1, 0))) => -expression.x
    case (expression, Constant(Vector2d(0, 1))) => expression.y
    case (expression, Constant(Vector2d(0, -1))) => -expression.y
    case (first, second) if (first == second) => first.squaredLength
    case (first, second) if (first == -second) => -first.squaredLength
    case _ => Expression1d.DotProduct2d(this, that)
  }

  final def dot(vector: Vector2d): Expression1d[P] =
    dot(Constant[P](vector))

  final def cross(that: VectorExpression2d[P]): Expression1d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) =>
      Expression1d.Constant(firstVector.cross(secondVector))
    case (Constant(Vector2d.Zero), _) => Expression1d.Constant(0)
    case (_, Constant(Vector2d.Zero)) => Expression1d.Constant(0)
    case (Constant(Vector2d(1, 0)), expression) => expression.y
    case (Constant(Vector2d(-1, 0)), expression) => -expression.y
    case (Constant(Vector2d(0, 1)), expression) => -expression.x
    case (Constant(Vector2d(0, -1)), expression) => expression.x
    case (expression, Constant(Vector2d(1, 0))) => -expression.y
    case (expression, Constant(Vector2d(-1, 0))) => expression.y
    case (expression, Constant(Vector2d(0, 1))) => expression.x
    case (expression, Constant(Vector2d(0, -1))) => -expression.x
    case (first, second) if (first == second) => Expression1d.Constant(0)
    case (first, second) if (first == -second) => Expression1d.Constant(0)
    case _ => Expression1d.CrossProduct2d(this, that)
  }

  final def cross(vector: Vector2d): Expression1d[P] =
    cross(Constant[P](vector))

  final def componentIn(direction: Direction2d): Expression1d[P] =
    dot(direction.vector)

  def x: Expression1d[P] =
    Expression1d.VectorXComponent2d(this)

  def y: Expression1d[P] =
    Expression1d.VectorYComponent2d(this)
}

object VectorExpression2d {
  def fromComponents[P](
    xExpression: Expression1d[P],
    yExpression: Expression1d[P]
  ): VectorExpression2d[P] = (xExpression, yExpression) match {
    case (Expression1d.Constant(xValue), Expression1d.Constant(yValue)) =>
      Constant(Vector2d(xValue, yValue))
    case _ => FromComponents(xExpression, yExpression)
  }

  case class Constant[P](val vector: Vector2d) extends VectorExpression2d[P] {
    override def derivative(parameter: P): VectorExpression2d[P] =
      Constant(Vector2d.Zero)

    override def unary_- : VectorExpression2d[P] =
      Constant(-vector)

    override def squaredLength: Expression1d[P] =
      Expression1d.Constant(vector.squaredLength)

    override def length: Expression1d[P] =
      Expression1d.Constant(vector.length)

    override def x: Expression1d[P] =
      Expression1d.Constant(vector.x)

    override def y: Expression1d[P] =
      Expression1d.Constant(vector.y)
  }

  case class FromComponents[P](
    override val x: Expression1d[P],
    override val y: Expression1d[P]
  ) extends VectorExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      VectorExpression2d.fromComponents(x.derivative(parameter), y.derivative(parameter))

    override def unary_- : VectorExpression2d[P] =
      VectorExpression2d.fromComponents(-x, -y)

    override def squaredLength: Expression1d[P] =
      x.squared + y.squared
  }

  case class Negation[P](expression: VectorExpression2d[P]) extends VectorExpression2d[P] {
    override def derivative(parameter: P): VectorExpression2d[P] =
      -expression.derivative(parameter)

    override def squaredLength: Expression1d[P] =
      expression.squaredLength

    override def length: Expression1d[P] =
      expression.length

    override def x: Expression1d[P] =
      -expression.x

    override def y: Expression1d[P] =
      -expression.y
  }

  case class Sum[P](
    firstExpression: VectorExpression2d[P],
    secondExpression: VectorExpression2d[P]
  ) extends VectorExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      firstExpression.derivative(parameter) + secondExpression.derivative(parameter)

    override def x: Expression1d[P] =
      firstExpression.x + secondExpression.x

    override def y: Expression1d[P] =
      firstExpression.y + secondExpression.y
  }

  case class Difference[P](
    firstExpression: VectorExpression2d[P],
    secondExpression: VectorExpression2d[P]
  ) extends VectorExpression2d[P] {

    override def unary_- : VectorExpression2d[P] =
      Difference[P](secondExpression, firstExpression)

    override def derivative(parameter: P): VectorExpression2d[P] =
      firstExpression.derivative(parameter) - secondExpression.derivative(parameter)

    override def x: Expression1d[P] =
      firstExpression.x - secondExpression.x

    override def y: Expression1d[P] =
      firstExpression.y - secondExpression.y
  }

  case class Displacement[P](
    firstExpression: PointExpression2d[P],
    secondExpression: PointExpression2d[P]
  ) extends VectorExpression2d[P] {

    override def unary_- : VectorExpression2d[P] =
      Displacement[P](secondExpression, firstExpression)

    override def derivative(parameter: P): VectorExpression2d[P] =
      secondExpression.derivative(parameter) - firstExpression.derivative(parameter)

    override def x: Expression1d[P] =
      secondExpression.x - firstExpression.x

    override def y: Expression1d[P] =
      secondExpression.y - firstExpression.y
  }

  case class Product[P](
    scalarExpression: Expression1d[P],
    vectorExpression: VectorExpression2d[P]
  ) extends VectorExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      scalarExpression.derivative(parameter) * vectorExpression +
      scalarExpression * vectorExpression.derivative(parameter)

    override def x: Expression1d[P] =
      scalarExpression * vectorExpression.x

    override def y: Expression1d[P] =
      scalarExpression * vectorExpression.y
  }

  case class Quotient[P](
    vectorExpression: VectorExpression2d[P],
    scalarExpression: Expression1d[P]
  ) extends VectorExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      ( vectorExpression.derivative(parameter) * scalarExpression -
        vectorExpression * scalarExpression.derivative(parameter) ) /
      scalarExpression.squared

    override def x: Expression1d[P] =
      vectorExpression.x / scalarExpression

    override def y: Expression1d[P] =
      vectorExpression.y / scalarExpression
  }
}
