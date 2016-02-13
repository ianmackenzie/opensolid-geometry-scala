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

  def condition: ScalarExpression[P]

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
}

object ScalarExpression {
  def sqrt[P](expression: ScalarExpression[P]): ScalarExpression[P] =
    SquareRoot(expression)

  def sin[P](expression: ScalarExpression[P]): ScalarExpression[P] =
    Sine(expression)

  def cos[P](expression: ScalarExpression[P]): ScalarExpression[P] =
    Cosine(expression)

  def tan[P](expression: ScalarExpression[P]): ScalarExpression[P] =
    Tangent(expression)

  def asin[P](expression: ScalarExpression[P]): ScalarExpression[P] =
    Arcsine(expression)

  def acos[P](expression: ScalarExpression[P]): ScalarExpression[P] =
    Arccosine(expression)

  def atan[P](expression: ScalarExpression[P]): ScalarExpression[P] =
    Arctangent(expression)

  abstract class Parameter[P <: Parameter[P]] extends ScalarExpression[P] {
    def index: Int

    override def derivative(parameter: P): ScalarExpression[P] =
      if (parameter eq this) Constant(1) else Constant(0)

    override def condition: ScalarExpression[P] =
      Constant(1)
  }

  case class Constant[P](val value: Double) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      Constant(0)

    override def condition: ScalarExpression[P] =
      Constant(1)

    override def unary_- : ScalarExpression[P] =
      Constant(-value)

    override def squared: ScalarExpression[P] =
      Constant(value * value)
  }

  case class Negation[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      -expression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      expression.condition

    override def unary_- : ScalarExpression[P] =
      expression

    override def squared: ScalarExpression[P] =
      expression.squared
  }

  case class Sum[P](
    firstExpression: ScalarExpression[P],
    secondExpression: ScalarExpression[P]
  ) extends ScalarExpression[P] {

    override def equals(other: Any): Boolean = other match {
      case Sum(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int =
      firstExpression.hashCode * secondExpression.hashCode

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter) + secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition
  }

  case class Difference[P](
    firstExpression: ScalarExpression[P],
    secondExpression: ScalarExpression[P]
  ) extends ScalarExpression[P] {

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter) - secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition

    override def unary_- : ScalarExpression[P] =
      Difference[P](secondExpression, firstExpression)
  }

  case class Product[P](
    firstExpression: ScalarExpression[P],
    secondExpression: ScalarExpression[P]
  ) extends ScalarExpression[P] {

    override def equals(other: Any): Boolean = other match {
      case Product(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int =
      firstExpression.hashCode * secondExpression.hashCode

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter) * secondExpression +
      firstExpression * secondExpression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition
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

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression
  }

  case class Square[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      2 * expression * expression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class VectorXComponent2d[P](expression: VectorExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).x

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class VectorYComponent2d[P](expression: VectorExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).y

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class VectorXComponent3d[P](expression: VectorExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).x

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class VectorYComponent3d[P](expression: VectorExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).y

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class VectorZComponent3d[P](expression: VectorExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).z

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class PointXComponent2d[P](expression: PointExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).x

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class PointYComponent2d[P](expression: PointExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).y

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class PointXComponent3d[P](expression: PointExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).x

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class PointYComponent3d[P](expression: PointExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).y

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class PointZComponent3d[P](expression: PointExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter).z

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class DotProduct2d[P](
    firstExpression: VectorExpression2d[P],
    secondExpression: VectorExpression2d[P]
  ) extends ScalarExpression[P] {

    override def equals(other: Any): Boolean = other match {
      case DotProduct2d(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int =
      firstExpression.hashCode * secondExpression.hashCode

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter).dot(secondExpression) +
      firstExpression.dot(secondExpression.derivative(parameter))

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition
  }

  case class DotProduct3d[P](
    firstExpression: VectorExpression3d[P],
    secondExpression: VectorExpression3d[P]
  ) extends ScalarExpression[P] {

    override def equals(other: Any): Boolean = other match {
      case DotProduct3d(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int =
      firstExpression.hashCode * secondExpression.hashCode

    override def derivative(parameter: P): ScalarExpression[P] =
      firstExpression.derivative(parameter).dot(secondExpression) +
      firstExpression.dot(secondExpression.derivative(parameter))

    override def condition: ScalarExpression[P] =
      firstExpression.condition * secondExpression.condition
  }

  case class SquaredLength2d[P](expression: VectorExpression2d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      2 * expression.dot(expression.derivative(parameter))

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class SquaredLength3d[P](expression: VectorExpression3d[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      2 * expression.dot(expression.derivative(parameter))

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class SquareRoot[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter) / (2 * this)

    override def condition: ScalarExpression[P] =
      expression.condition * expression
  }

  case class Sine[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      ScalarExpression.cos(expression) * expression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class Cosine[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      -ScalarExpression.sin(expression) * expression.derivative(parameter)

    override def condition: ScalarExpression[P] =
      expression.condition
  }

  case class Tangent[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter) / ScalarExpression.cos(expression).squared

    override def condition: ScalarExpression[P] =
      expression.condition * ScalarExpression.cos(expression)
  }

  case class Arcsine[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter) / ScalarExpression.sqrt(1 - expression.squared)

    override def condition: ScalarExpression[P] =
      expression.condition * (1 - expression.squared)
  }

  case class Arccosine[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      -expression.derivative(parameter) / ScalarExpression.sqrt(1 - expression.squared)

    override def condition: ScalarExpression[P] =
      expression.condition * (1 - expression.squared)
  }

  case class Arctangent[P](expression: ScalarExpression[P]) extends ScalarExpression[P] {
    override def derivative(parameter: P): ScalarExpression[P] =
      expression.derivative(parameter) / (1 + expression.squared)

    override def condition: ScalarExpression[P] =
      expression.condition
  }
}
