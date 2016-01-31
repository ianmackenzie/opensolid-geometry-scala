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

sealed abstract class Expression1d[T] {
  import Expression1d._

  def derivative(parameter: T): Expression1d[T]

  def condition: Expression1d[T]

  def unary_- : Expression1d[T] = Negation(this)

  final def negated: Expression1d[T] = -this

  final def +(that: Expression1d[T]): Expression1d[T] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue + secondValue)
    case (expression, Constant(0)) => expression
    case (Constant(0), expression) => expression
    case (first, second) if (first == second) => Constant(2) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def plus(that: Expression1d[T]): Expression1d[T] = this + that

  final def +(value: Double): Expression1d[T] = this + Constant[T](value)

  final def plus(value: Double): Expression1d[T] = this + value

  final def -(that: Expression1d[T]): Expression1d[T] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue - secondValue)
    case (expression, Constant(0)) => expression
    case (Constant(0), expression) => -expression
    case (first, second) if (first == second) => Constant(0)
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def minus(that: Expression1d[T]): Expression1d[T] = this - that

  final def -(value: Double): Expression1d[T] = this - Constant[T](value)

  final def minus(value: Double): Expression1d[T] = this - value

  final def *(that: Expression1d[T]): Expression1d[T] = (this, that) match {
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

  final def times(that: Expression1d[T]): Expression1d[T] = this * that

  final def *(value: Double): Expression1d[T] = this * Constant[T](value)

  final def times(value: Double): Expression1d[T] = this * value

  final def *(that: Expression2d[T]): Expression2d[T] = (this, that) match {
    case (Constant(multiplier), Expression2d.Constant(x, y)) =>
      Expression2d.Constant(multiplier * x, multiplier * y)
    case (_, Expression2d.Constant(0, 0)) => Expression2d.Constant(0, 0)
    case (Constant(0), _) => Expression2d.Constant(0, 0)
    case (Constant(1), expression) => expression
    case (Constant(-1), expression) => -expression
    case (Quotient(a, b), Expression2d.Quotient(c, d)) => (a * c) / (b * d)
    case _ => Expression2d.Product(this, that)
  }

  final def times(that: Expression2d[T]): Expression2d[T] = this * that

  final def /(that: Expression1d[T]): Expression1d[T] = (this, that) match {
    case (_, Constant(0)) => throw new ArithmeticException("Division by zero")
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue / secondValue)
    case (Constant(0), _) => Constant(0)
    case (expression, Constant(1)) => expression
    case (expression, Constant(-1)) => -expression
    case (expression, Constant(value)) => Constant(1 / value) * expression
    case (expression, Quotient(numerator, denominator)) => expression * denominator / numerator
    case _ => Quotient(this, that)
  }

  final def dividedBy(that: Expression1d[T]): Expression1d[T] = this / that

  final def /(value: Double): Expression1d[T] = this / Constant[T](value)

  final def dividedBy(value: Double): Expression1d[T] = this / value

  def squared: Expression1d[T] = Square(this)
}

object Expression1d {
  def sqrt[T](expression: Expression1d[T]): Expression1d[T] = SquareRoot(expression)

  def sin[T](expression: Expression1d[T]): Expression1d[T] = Sine(expression)

  def cos[T](expression: Expression1d[T]): Expression1d[T] = Cosine(expression)

  def tan[T](expression: Expression1d[T]): Expression1d[T] = Tangent(expression)

  def asin[T](expression: Expression1d[T]): Expression1d[T] = Arcsine(expression)

  def acos[T](expression: Expression1d[T]): Expression1d[T] = Arccosine(expression)

  def atan[T](expression: Expression1d[T]): Expression1d[T] = Arctangent(expression)

  sealed abstract class Parameter[T] extends Expression1d[T] {
    val index: Int

    override def condition: Expression1d[T] = Constant(1)
  }

  object Parameter {
    def unapply(parameter: Parameter[_]): Option[Int] = Some(parameter.index)
  }

  sealed abstract class CurveParameter extends Parameter[CurveParameter] {
    override val index: Int = 0

    override def derivative(parameter: CurveParameter): Expression1d[CurveParameter] = Constant(1)
  }

  object T extends CurveParameter

  sealed abstract class SurfaceParameter extends Parameter[SurfaceParameter] {
    override def derivative(parameter: SurfaceParameter): Expression1d[SurfaceParameter] =
      if (parameter == this) Constant(1) else Constant(0)
  }

  object U extends SurfaceParameter {
    override val index = 0
  }

  object V extends SurfaceParameter {
    override val index = 1
  }

  case class Constant[T](val value: Double) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] = Constant(0)

    override def condition: Expression1d[T] = Constant(1)

    override def unary_- : Expression1d[T] = Constant(-value)

    override def squared: Expression1d[T] = Constant(value * value)
  }

  case class Negation[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] = -expression.derivative(parameter)

    override def condition: Expression1d[T] = expression.condition

    override def unary_- : Expression1d[T] = expression

    override def squared: Expression1d[T] = expression.squared
  }

  case class Sum[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
    extends Expression1d[T] {

    override def equals(other: Any): Boolean = other match {
      case Sum(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int = firstExpression.hashCode * secondExpression.hashCode

    override def derivative(parameter: T): Expression1d[T] =
      firstExpression.derivative(parameter) + secondExpression.derivative(parameter)

    override def condition: Expression1d[T] = firstExpression.condition * secondExpression.condition
  }

  case class Difference[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
    extends Expression1d[T] {

    override def derivative(parameter: T): Expression1d[T] =
      firstExpression.derivative(parameter) - secondExpression.derivative(parameter)

    override def condition: Expression1d[T] = firstExpression.condition * secondExpression.condition

    override def unary_- : Expression1d[T] =
      Difference[T](secondExpression, firstExpression)
  }

  case class Product[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
    extends Expression1d[T] {

    override def equals(other: Any): Boolean = other match {
      case Product(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int = firstExpression.hashCode * secondExpression.hashCode

    override def derivative(parameter: T): Expression1d[T] =
      firstExpression.derivative(parameter) * secondExpression +
      firstExpression * secondExpression.derivative(parameter)

    override def condition: Expression1d[T] = firstExpression.condition * secondExpression.condition
  }

  case class Quotient[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
    extends Expression1d[T] {

    override def derivative(parameter: T): Expression1d[T] =
      (
        firstExpression.derivative(parameter) * secondExpression -
        firstExpression * secondExpression.derivative(parameter)
      ) / secondExpression.squared

    override def condition: Expression1d[T] = firstExpression.condition * secondExpression
  }

  case class Square[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] =
      2 * expression * expression.derivative(parameter)

    override def condition: Expression1d[T] = expression.condition
  }

  case class XComponent2d[T](expression: Expression2d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] = expression.derivative(parameter).x

    override def condition: Expression1d[T] = expression.condition
  }

  case class YComponent2d[T](expression: Expression2d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] = expression.derivative(parameter).y

    override def condition: Expression1d[T] = expression.condition
  }

  case class DotProduct2d[T](firstExpression: Expression2d[T], secondExpression: Expression2d[T])
    extends Expression1d[T] {

    override def equals(other: Any): Boolean = other match {
      case DotProduct2d(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int = firstExpression.hashCode * secondExpression.hashCode

    override def derivative(parameter: T): Expression1d[T] =
      firstExpression.derivative(parameter).dot(secondExpression) +
      firstExpression.dot(secondExpression.derivative(parameter))

    override def condition: Expression1d[T] = firstExpression.condition * secondExpression.condition
  }

  case class SquaredNorm2d[T](expression: Expression2d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] =
      2 * expression.dot(expression.derivative(parameter))

    override def condition: Expression1d[T] = expression.condition
  }

  case class SquareRoot[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] =
      expression.derivative(parameter) / (2 * this)

    override def condition: Expression1d[T] = expression.condition * expression
  }

  case class Sine[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] =
      Expression1d.cos(expression) * expression.derivative(parameter)

    override def condition: Expression1d[T] = expression.condition
  }

  case class Cosine[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] =
      -Expression1d.sin(expression) * expression.derivative(parameter)

    override def condition: Expression1d[T] = expression.condition
  }

  case class Tangent[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] =
      expression.derivative(parameter) / Expression1d.cos(expression).squared

    override def condition: Expression1d[T] =
      expression.condition * Expression1d.cos(expression)
  }

  case class Arcsine[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] =
      expression.derivative(parameter) / Expression1d.sqrt(1 - expression.squared)

    override def condition: Expression1d[T] = expression.condition * (1 - expression.squared)
  }

  case class Arccosine[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] =
      -expression.derivative(parameter) / Expression1d.sqrt(1 - expression.squared)

    override def condition: Expression1d[T] = expression.condition * (1 - expression.squared)
  }

  case class Arctangent[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(parameter: T): Expression1d[T] =
      expression.derivative(parameter) / (1 + expression.squared)

    override def condition: Expression1d[T] = expression.condition
  }
}
