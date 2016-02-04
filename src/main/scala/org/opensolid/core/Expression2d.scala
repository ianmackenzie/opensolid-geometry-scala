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

sealed abstract class Expression2d[P] {
  import Expression2d._

  def derivative(parameter: P): Expression2d[P]

  def condition: ScalarExpression[P]

  def unary_- : Expression2d[P] =
    Negation(this)

  final def negated: Expression2d[P] =
    -this

  final def +(that: Expression2d[P]): Expression2d[P] = (this, that) match {
    case (Constant(x1, y1), Constant(x2, y2)) => Constant(x1 + x2, y1 + y2)
    case (expression, Constant(0, 0)) => expression
    case (Constant(0, 0), expression) => expression
    case (first, second) if (first == second) => ScalarExpression.Constant(2) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def plus(that: Expression2d[P]): Expression2d[P] =
    this + that

  final def -(that: Expression2d[P]): Expression2d[P] = (this, that) match {
    case (Constant(x1, y1), Constant(x2, y2)) => Constant(x1 - x2, y1 - y2)
    case (expression, Constant(0, 0)) => expression
    case (Constant(0, 0), expression) => -expression
    case (first, second) if (first == second) => Constant(0, 0)
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def minus(that: Expression2d[P]): Expression2d[P] =
    this - that

  final def *(that: ScalarExpression[P]): Expression2d[P] =
    that * this

  final def *(value: Double): Expression2d[P] =
    this * ScalarExpression.Constant[P](value)

  final def times(that: ScalarExpression[P]): Expression2d[P] =
    this * that

  final def times(value: Double): Expression2d[P] =
    this * value

  final def /(that: ScalarExpression[P]): Expression2d[P] = (this, that) match {
    case (_, ScalarExpression.Constant(0)) => throw new ArithmeticException("Division by zero")
    case (Constant(x, y), ScalarExpression.Constant(divisor)) => Constant(x / divisor, y / divisor)
    case (Constant(0, 0), _) => Constant(0, 0)
    case (expression, ScalarExpression.Constant(1)) => expression
    case (expression, ScalarExpression.Constant(-1)) => -expression
    case (expression, ScalarExpression.Constant(value)) =>
      ScalarExpression.Constant(1 / value) * expression
    case (expression, ScalarExpression.Quotient(numerator, denominator)) =>
      expression * (denominator / numerator)
    case _ => Quotient(this, that)
  }

  final def /(value: Double): Expression2d[P] =
    this / ScalarExpression.Constant[P](value)

  final def dividedBy(that: ScalarExpression[P]): Expression2d[P] =
    this / that

  final def dividedBy(value: Double): Expression2d[P] =
    this / value

  def squaredNorm: ScalarExpression[P] =
    ScalarExpression.SquaredNorm2d(this)

  def norm: ScalarExpression[P] =
    ScalarExpression.sqrt(squaredNorm)

  final def dot(that: Expression2d[P]): ScalarExpression[P] = (this, that) match {
    case (Constant(x1, y1), Constant(x2, y2)) => ScalarExpression.Constant(x1 * x2 + y1 * y2)
    case (Constant(0, 0), _) => ScalarExpression.Constant(0)
    case (_, Constant(0, 0)) => ScalarExpression.Constant(0)
    case (Constant(1, 0), expression) => expression.x
    case (Constant(0, 1), expression) => expression.y
    case (expression, Constant(1, 0)) => expression.x
    case (expression, Constant(0, 1)) => expression.y
    case (first, second) if (first == second) => first.squaredNorm
    case _ => ScalarExpression.DotProduct2d(this, that)
  }

  def x: ScalarExpression[P] =
    ScalarExpression.XComponent2d(this)

  def y: ScalarExpression[P] =
    ScalarExpression.YComponent2d(this)
}

object Expression2d {
  def fromComponents[P](
    x: ScalarExpression[P],
    y: ScalarExpression[P]
  ): Expression2d[P] = (x, y) match {
    case (ScalarExpression.Constant(xValue), ScalarExpression.Constant(yValue)) =>
      Constant(xValue, yValue)
    case _ => FromComponents(x, y)
  }

  case class Constant[P](val xValue: Double, val yValue: Double) extends Expression2d[P] {
    override def derivative(parameter: P): Expression2d[P] =
      Constant(0, 0)

    override def condition: ScalarExpression[P] =
      ScalarExpression.Constant(1)

    override def unary_- : Expression2d[P] =
      Constant(-xValue, -yValue)

    override def squaredNorm: ScalarExpression[P] =
      ScalarExpression.Constant(xValue * xValue + yValue * yValue)

    override def norm: ScalarExpression[P] =
      ScalarExpression.Constant(math.sqrt(xValue * xValue + yValue * yValue))

    override def x: ScalarExpression[P] =
      ScalarExpression.Constant(xValue)

    override def y: ScalarExpression[P] =
      ScalarExpression.Constant(yValue)
  }

  case class FromComponents[P](override val x: ScalarExpression[P], override val y: ScalarExpression[P])
    extends Expression2d[P] {

    override def derivative(parameter: P): Expression2d[P] =
      Expression2d.fromComponents(x.derivative(parameter), y.derivative(parameter))

    override def condition: ScalarExpression[P] =
      x.condition * y.condition

    override def unary_- : Expression2d[P] =
      Expression2d.fromComponents(-x, -y)

    override def squaredNorm: ScalarExpression[P] =
      x.squared + y.squared
  }

  case class Negation[P](expression: Expression2d[P]) extends Expression2d[P] {
    override def derivative(parameter: P): Expression2d[P] =
      -expression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      expression.condition

    override def squaredNorm: ScalarExpression[P] =
      expression.squaredNorm

    override def norm: ScalarExpression[P] =
      expression.norm

    override def x: ScalarExpression[P] =
      -expression.x

    override def y: ScalarExpression[P] =
      -expression.y
  }

  case class Sum[P](firstExpression: Expression2d[P], secondExpression: Expression2d[P])
    extends Expression2d[P] {

    override def equals(other: Any): Boolean = other match {
      case Sum(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int =
      firstExpression.hashCode * secondExpression.hashCode

    override def derivative(parameter: P): Expression2d[P] =
      firstExpression.derivative(parameter) + secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression.x + secondExpression.x

    override def y: ScalarExpression[P] =
      firstExpression.y + secondExpression.y
  }

  case class Difference[P](firstExpression: Expression2d[P], secondExpression: Expression2d[P])
    extends Expression2d[P] {

    override def unary_- : Expression2d[P] =
      Difference[P](secondExpression, firstExpression)

    override def derivative(parameter: P): Expression2d[P] =
      firstExpression.derivative(parameter) - secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression.x - secondExpression.x

    override def y: ScalarExpression[P] =
      firstExpression.y - secondExpression.y
  }

  case class Product[P](firstExpression: ScalarExpression[P], secondExpression: Expression2d[P])
    extends Expression2d[P] {

    override def derivative(parameter: P): Expression2d[P] =
      firstExpression.derivative(parameter) * secondExpression +
      firstExpression * secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def x: ScalarExpression[P] =
      firstExpression * secondExpression.x

    override def y: ScalarExpression[P] =
      firstExpression * secondExpression.y
  }

  case class Quotient[P](firstExpression: Expression2d[P], secondExpression: ScalarExpression[P])
    extends Expression2d[P] {

    override def derivative(parameter: P): Expression2d[P] =
      (
        firstExpression.derivative(parameter) * secondExpression -
        firstExpression * secondExpression.derivative(parameter)
      ) / secondExpression.squared

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression

    override def x: ScalarExpression[P] =
      firstExpression.x / secondExpression

    override def y: ScalarExpression[P] =
      firstExpression.y / secondExpression
  }
}
