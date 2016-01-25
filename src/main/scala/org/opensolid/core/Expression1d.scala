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

  def derivative(index: Int): Expression1d[T]

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

  sealed abstract class Identity[T] extends Expression1d[T]

  object Identity extends Identity[Double] {
    override def derivative(index: Int): Expression1d[Double] = {
      assert(index == 0)
      Constant(1)
    }
  }

  case class Constant[T](val value: Double) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] = Constant(0)

    override def unary_- : Expression1d[T] = Constant(-value)

    override def squared: Expression1d[T] = Constant(value * value)
  }

  case class Negation[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] = -expression.derivative(index)

    override def unary_- : Expression1d[T] = expression

    override def squared: Expression1d[T] = expression.squared
  }

  case class Sum[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
    extends Expression1d[T] {

    override def derivative(index: Int): Expression1d[T] =
      firstExpression.derivative(index) + secondExpression.derivative(index)

    override def equals(other: Any): Boolean = other match {
      case Sum(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int = firstExpression.hashCode * secondExpression.hashCode
  }

  case class Difference[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
    extends Expression1d[T] {

    override def derivative(index: Int): Expression1d[T] =
      firstExpression.derivative(index) - secondExpression.derivative(index)

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

    override def derivative(index: Int): Expression1d[T] =
      firstExpression.derivative(index) * secondExpression +
      firstExpression * secondExpression.derivative(index)
  }

  case class Quotient[T](firstExpression: Expression1d[T], secondExpression: Expression1d[T])
    extends Expression1d[T] {

    override def derivative(index: Int): Expression1d[T] =
      (
        firstExpression.derivative(index) * secondExpression -
        firstExpression * secondExpression.derivative(index)
      ) / secondExpression.squared
  }

  case class Square[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] =
      2 * expression * expression.derivative(index)
  }

  case class XComponent2d[T](expression: Expression2d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] = expression.derivative(index).component(0)
  }

  case class YComponent2d[T](expression: Expression2d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] = expression.derivative(index).component(1)
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

    override def derivative(index: Int): Expression1d[T] =
      firstExpression.derivative(index).dot(secondExpression) +
      firstExpression.dot(secondExpression.derivative(index))
  }

  case class SquaredNorm2d[T](expression: Expression2d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] =
      2 * expression.dot(expression.derivative(index))
  }

  case class SquareRoot[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] =
      expression.derivative(index) / (2 * this)
  }

  case class Sine[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] =
      Expression1d.cos(expression) * expression.derivative(index)
  }

  case class Cosine[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] =
      -Expression1d.sin(expression) * expression.derivative(index)
  }

  case class Tangent[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] =
      expression.derivative(index) / Expression1d.cos(expression).squared
  }

  case class Arcsine[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] =
      expression.derivative(index) / Expression1d.sqrt(1 - expression.squared)
  }

  case class Arccosine[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] =
      -expression.derivative(index) / Expression1d.sqrt(1 - expression.squared)
  }

  case class Arctangent[T](expression: Expression1d[T]) extends Expression1d[T] {
    override def derivative(index: Int): Expression1d[T] =
      expression.derivative(index) / (1 + expression.squared)
  }
}
