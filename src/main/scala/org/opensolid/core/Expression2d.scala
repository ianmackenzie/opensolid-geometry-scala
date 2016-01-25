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

sealed abstract class Expression2d[T] {
  import Expression2d._

  def derivative(index: Int): Expression2d[T]

  def unary_- : Expression2d[T] = Negation(this)

  final def negated: Expression2d[T] = -this

  final def +(that: Expression2d[T]): Expression2d[T] = (this, that) match {
    case (Constant(x1, y1), Constant(x2, y2)) => Constant(x1 + x2, y1 + y2)
    case (expression, Constant(0, 0)) => expression
    case (Constant(0, 0), expression) => expression
    case (first, second) if (first == second) => Expression1d.Constant(2) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def plus(that: Expression2d[T]): Expression2d[T] = this + that

  final def -(that: Expression2d[T]): Expression2d[T] = (this, that) match {
    case (Constant(x1, y1), Constant(x2, y2)) => Constant(x1 - x2, y1 - y2)
    case (expression, Constant(0, 0)) => expression
    case (Constant(0, 0), expression) => -expression
    case (first, second) if (first == second) => Constant(0, 0)
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def minus(that: Expression2d[T]): Expression2d[T] = this - that

  final def *(that: Expression1d[T]): Expression2d[T] = that * this

  final def times(that: Expression1d[T]): Expression2d[T] = this * that

  final def *(value: Double): Expression2d[T] = this * Expression1d.Constant[T](value)

  final def times(value: Double): Expression2d[T] = this * value

  final def /(that: Expression1d[T]): Expression2d[T] = (this, that) match {
    case (_, Expression1d.Constant(0)) => throw new ArithmeticException("Division by zero")
    case (Constant(x, y), Expression1d.Constant(divisor)) => Constant(x / divisor, y / divisor)
    case (Constant(0, 0), _) => Constant(0, 0)
    case (expression, Expression1d.Constant(1)) => expression
    case (expression, Expression1d.Constant(-1)) => -expression
    case (expression, Expression1d.Constant(value)) => Expression1d.Constant(1 / value) * expression
    case (expression, Expression1d.Quotient(numerator, denominator)) =>
      expression * (denominator / numerator)
    case _ => Quotient(this, that)
  }

  final def dividedBy(that: Expression1d[T]): Expression2d[T] = this / that

  final def /(value: Double): Expression2d[T] = this / Expression1d.Constant[T](value)

  final def dividedBy(value: Double): Expression2d[T] = this / value

  def squaredNorm: Expression1d[T] = Expression1d.SquaredNorm2d(this)

  def norm: Expression1d[T] = Expression1d.SquareRoot(squaredNorm)

  final def dot(that: Expression2d[T]): Expression1d[T] = (this, that) match {
    case (Constant(x1, y1), Constant(x2, y2)) => Expression1d.Constant(x1 * x2 + y1 * y2)
    case (Constant(0, 0), _) => Expression1d.Constant(0)
    case (_, Constant(0, 0)) => Expression1d.Constant(0)
    case (Constant(1, 0), expression) => expression.component(0)
    case (Constant(0, 1), expression) => expression.component(1)
    case (expression, Constant(1, 0)) => expression.component(0)
    case (expression, Constant(0, 1)) => expression.component(1)
    case (first, second) if (first == second) => first.squaredNorm
    case _ => Expression1d.DotProduct2d(this, that)
  }

  def component(index: Int): Expression1d[T] = index match {
    case 0 => Expression1d.XComponent2d(this)
    case 1 => Expression1d.YComponent2d(this)
    case _ => throw componentIndexException
  }
}

object Expression2d {
  private[Expression2d] def componentIndexException =
    new IllegalArgumentException("Component index for Expression2d must be 0 or 1")

  sealed abstract class Identity[T] extends Expression2d[T]

  object Identity extends Identity[(Double, Double)] {
    override def derivative(index: Int): Expression2d[(Double, Double)] = index match {
      case 0 => Constant(1, 0)
      case 1 => Constant(0, 1)
      case _ =>
        throw new IllegalArgumentException(
          "Derivative index to expression of two parameters must be 0 or 1"
        )
    }
  }

  case class Constant[T](val x: Double, val y: Double) extends Expression2d[T] {
    override def derivative(index: Int): Expression2d[T] = Constant(0, 0)

    override def unary_- : Expression2d[T] = Constant(-x, -y)

    override def squaredNorm: Expression1d[T] = Expression1d.Constant(x * x + y * y)

    override def norm: Expression1d[T] = Expression1d.Constant(math.sqrt(x * x + y * y))

    override def component(index: Int): Expression1d[T] = index match {
      case 0 => Expression1d.Constant(x)
      case 1 => Expression1d.Constant(y)
      case _ => throw componentIndexException
    }
  }

  case class Negation[T](expression: Expression2d[T]) extends Expression2d[T] {
    override def derivative(index: Int): Expression2d[T] = -expression.derivative(index)

    override def squaredNorm: Expression1d[T] = expression.squaredNorm

    override def norm: Expression1d[T] = expression.norm

    override def component(index: Int): Expression1d[T] = -expression.component(index)
  }

  case class Sum[T](firstExpression: Expression2d[T], secondExpression: Expression2d[T])
    extends Expression2d[T] {

    override def equals(other: Any): Boolean = other match {
      case Sum(otherFirst, otherSecond) =>
        (firstExpression == otherFirst && secondExpression == otherSecond) ||
        (firstExpression == otherSecond && secondExpression == otherFirst)
      case _ => false
    }

    override def hashCode: Int = firstExpression.hashCode * secondExpression.hashCode

    override def derivative(index: Int): Expression2d[T] =
      firstExpression.derivative(index) + secondExpression.derivative(index)

    override def component(index: Int): Expression1d[T] =
      firstExpression.component(index) + secondExpression.component(index)
  }

  case class Difference[T](firstExpression: Expression2d[T], secondExpression: Expression2d[T])
    extends Expression2d[T] {

    override def unary_- : Expression2d[T] =
      Difference[T](secondExpression, firstExpression)

    override def derivative(index: Int): Expression2d[T] =
      firstExpression.derivative(index) - secondExpression.derivative(index)

    override def component(index: Int): Expression1d[T] =
      firstExpression.component(index) - secondExpression.component(index)
  }

  case class Product[T](firstExpression: Expression1d[T], secondExpression: Expression2d[T])
    extends Expression2d[T] {

    override def derivative(index: Int): Expression2d[T] =
      firstExpression.derivative(index) * secondExpression +
      firstExpression * secondExpression.derivative(index)

    override def component(index: Int): Expression1d[T] =
      firstExpression * secondExpression.component(index)
  }

  case class Quotient[T](firstExpression: Expression2d[T], secondExpression: Expression1d[T])
    extends Expression2d[T] {

    override def derivative(index: Int): Expression2d[T] =
      (
        firstExpression.derivative(index) * secondExpression -
        firstExpression * secondExpression.derivative(index)
      ) / secondExpression.squared

    override def component(index: Int): Expression1d[T] =
      firstExpression.component(index) / secondExpression
  }
}
