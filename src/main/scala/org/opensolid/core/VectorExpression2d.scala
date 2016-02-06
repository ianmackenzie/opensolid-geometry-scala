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

  def condition: ScalarExpression[P]

  def unary_- : VectorExpression2d[P] =
    Negation(this)

  final def negated: VectorExpression2d[P] =
    -this

  final def +(that: VectorExpression2d[P]): VectorExpression2d[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) => Constant(firstVector + secondVector)
    case (expression, Constant(Vector2d.Zero)) => expression
    case (Constant(Vector2d.Zero), expression) => expression
    case (first, second) if (first == second) => ScalarExpression.Constant(2) * first
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

  final def *(scalarExpression: ScalarExpression[P]): VectorExpression2d[P] =
    (this, scalarExpression) match {
      case (Constant(vector), ScalarExpression.Constant(value)) => Constant(vector * value)
      case (Constant(Vector2d.Zero), _) => Constant(Vector2d.Zero)
      case (_, ScalarExpression.Constant(0)) => Constant(Vector2d.Zero)
      case (expression, ScalarExpression.Constant(1)) => expression
      case (expression, ScalarExpression.Constant(-1)) => -expression
      case (Quotient(a, b), ScalarExpression.Quotient(c, d)) => (a * c) / (b * d)
      case _ => VectorExpression2d.Product(scalarExpression, this)
    }

  final def *(value: Double): VectorExpression2d[P] =
    this * ScalarExpression.Constant[P](value)

  final def times(scalarExpression: ScalarExpression[P]): VectorExpression2d[P] =
    this * scalarExpression

  final def times(value: Double): VectorExpression2d[P] =
    this * value

  final def /(scalarExpression: ScalarExpression[P]): VectorExpression2d[P] =
    (this, scalarExpression) match {
      case (_, ScalarExpression.Constant(0)) => throw new ArithmeticException("Division by zero")
      case (Constant(vector), ScalarExpression.Constant(divisor)) => Constant(vector / divisor)
      case (Constant(Vector2d.Zero), _) => Constant(Vector2d.Zero)
      case (expression, ScalarExpression.Constant(1)) => expression
      case (expression, ScalarExpression.Constant(-1)) => -expression
      case (expression, ScalarExpression.Constant(value)) =>
        ScalarExpression.Constant(1 / value) * expression
      case (expression, ScalarExpression.Quotient(numerator, denominator)) =>
        expression * (denominator / numerator)
      case _ => Quotient(this, scalarExpression)
    }

  final def /(value: Double): VectorExpression2d[P] =
    this / ScalarExpression.Constant[P](value)

  final def dividedBy(scalarExpression: ScalarExpression[P]): VectorExpression2d[P] =
    this / scalarExpression

  final def dividedBy(value: Double): VectorExpression2d[P] =
    this / value

  def squaredLength: ScalarExpression[P] =
    ScalarExpression.SquaredLength2d(this)

  def length: ScalarExpression[P] =
    ScalarExpression.sqrt(squaredLength)

  final def dot(that: VectorExpression2d[P]): ScalarExpression[P] = (this, that) match {
    case (Constant(firstVector), Constant(secondVector)) =>
      ScalarExpression.Constant(firstVector.dot(secondVector))
    case (Constant(Vector2d.Zero), _) => ScalarExpression.Constant(0)
    case (_, Constant(Vector2d.Zero)) => ScalarExpression.Constant(0)
    case (Constant(Vector2d(1, 0)), expression) => expression.x
    case (Constant(Vector2d(0, 1)), expression) => expression.y
    case (expression, Constant(Vector2d(1, 0))) => expression.x
    case (expression, Constant(Vector2d(0, 1))) => expression.y
    case (first, second) if (first == second) => first.squaredLength
    case _ => ScalarExpression.DotProduct2d(this, that)
  }

  def x: ScalarExpression[P] =
    ScalarExpression.VectorXComponent2d(this)

  def y: ScalarExpression[P] =
    ScalarExpression.VectorYComponent2d(this)
}

object VectorExpression2d {
  def fromComponents[P](
    xExpression: ScalarExpression[P],
    yExpression: ScalarExpression[P]
  ): VectorExpression2d[P] = (xExpression, yExpression) match {
    case (ScalarExpression.Constant(xValue), ScalarExpression.Constant(yValue)) =>
      Constant(Vector2d(xValue, yValue))
    case _ => FromComponents(xExpression, yExpression)
  }

  case class Constant[P](val vector: Vector2d) extends VectorExpression2d[P] {
    override def derivative(parameter: P): VectorExpression2d[P] =
      Constant(Vector2d.Zero)

    override def condition: ScalarExpression[P] =
      ScalarExpression.Constant(1)

    override def unary_- : VectorExpression2d[P] =
      Constant(-vector)

    override def squaredLength: ScalarExpression[P] =
      ScalarExpression.Constant(vector.squaredLength)

    override def length: ScalarExpression[P] =
      ScalarExpression.Constant(vector.length)

    override def x: ScalarExpression[P] =
      ScalarExpression.Constant(vector.x)

    override def y: ScalarExpression[P] =
      ScalarExpression.Constant(vector.y)
  }

  case class FromComponents[P](
    override val x: ScalarExpression[P],
    override val y: ScalarExpression[P]
  ) extends VectorExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      VectorExpression2d.fromComponents(x.derivative(parameter), y.derivative(parameter))

    override def condition: ScalarExpression[P] =
      x.condition * y.condition

    override def unary_- : VectorExpression2d[P] =
      VectorExpression2d.fromComponents(-x, -y)

    override def squaredLength: ScalarExpression[P] =
      x.squared + y.squared
  }

  case class Negation[P](expression: VectorExpression2d[P]) extends VectorExpression2d[P] {
    override def derivative(parameter: P): VectorExpression2d[P] =
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
  }

  case class Sum[P](
    firstExpression: VectorExpression2d[P],
    secondExpression: VectorExpression2d[P]
  ) extends VectorExpression2d[P] {

    override def equals(other: Any): Boolean = other match {
      case Sum(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int =
      firstExpression.hashCode * secondExpression.hashCode

    override def derivative(parameter: P): VectorExpression2d[P] =
      firstExpression.derivative(parameter) + secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression.x + secondExpression.x

    override def y: ScalarExpression[P] =
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

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression.x - secondExpression.x

    override def y: ScalarExpression[P] =
      firstExpression.y - secondExpression.y
  }

  case class PointDifference[P](
    firstExpression: PointExpression2d[P],
    secondExpression: PointExpression2d[P]
  ) extends VectorExpression2d[P] {

    override def unary_- : VectorExpression2d[P] =
      PointDifference[P](secondExpression, firstExpression)

    override def derivative(parameter: P): VectorExpression2d[P] =
      firstExpression.derivative(parameter) - secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression.x - secondExpression.x

    override def y: ScalarExpression[P] =
      firstExpression.y - secondExpression.y
  }

  case class Product[P](
    scalarExpression: ScalarExpression[P],
    vectorExpression: VectorExpression2d[P]
  ) extends VectorExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
      scalarExpression.derivative(parameter) * vectorExpression +
      scalarExpression * vectorExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      scalarExpression.condition * vectorExpression.condition

    override def x: ScalarExpression[P] =
      scalarExpression * vectorExpression.x

    override def y: ScalarExpression[P] =
      scalarExpression * vectorExpression.y
  }

  case class Quotient[P](
    vectorExpression: VectorExpression2d[P],
    scalarExpression: ScalarExpression[P]
  ) extends VectorExpression2d[P] {

    override def derivative(parameter: P): VectorExpression2d[P] =
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
  }
}
