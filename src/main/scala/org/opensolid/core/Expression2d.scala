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

sealed abstract class Expression2d[T] {
  import Expression2d._

  def unary_- : Expression2d[T] = Negation(this)

  final def negated: Expression2d[T] = -this

  final def +(that: Expression2d[T]): Expression2d[T] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue + secondValue)
    case (expression, Constant(Vector2d.Zero)) => expression
    case (Constant(Vector2d.Zero), expression) => expression
    case (first, second) if (first == second) => Expression1d.Constant(2.0) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def plus(that: Expression2d[T]): Expression2d[T] = this + that

  final def -(that: Expression2d[T]): Expression2d[T] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue - secondValue)
    case (expression, Constant(Vector2d.Zero)) => expression
    case (Constant(Vector2d.Zero), expression) => -expression
    case (first, second) if (first == second) => Constant(Vector2d.Zero)
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def minus(that: Expression2d[T]): Expression2d[T] = this - that

  final def *(that: Expression1d[T]): Expression2d[T] = that * this

  final def times(that: Expression1d[T]): Expression2d[T] = this * that

  final def /(that: Expression1d[T]): Expression2d[T] = (this, that) match {
    case (_, Expression1d.Constant(0.0)) => throw new ArithmeticException("Division by zero")
    case (Constant(firstValue), Expression1d.Constant(secondValue)) =>
      Constant(firstValue / secondValue)
    case (Constant(Vector2d.Zero), _) => Constant(Vector2d.Zero)
    case (expression, Expression1d.Constant(1.0)) => expression
    case (expression, Expression1d.Constant(-1.0)) => -expression
    case (expression, Expression1d.Constant(value)) =>
      Expression1d.Constant(1.0 / value) * expression
    case (expression, Expression1d.Quotient(numerator, denominator)) =>
      expression * (denominator / numerator)
    case _ => Quotient(this, that)
  }

  final def dividedBy(that: Expression1d[T]): Expression2d[T] = this / that

  def squaredNorm: Expression1d[T] = Expression1d.SquaredNorm2d(this)

  def norm: Expression1d[T] = Expression1d.Norm2d(this)

  final def dot(that: Expression2d[T]): Expression1d[T] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) =>
      Expression1d.Constant(firstValue.dot(secondValue))
    case (Constant(Vector2d.Zero), _) => Expression1d.Constant(0.0)
    case (_, Constant(Vector2d.Zero)) => Expression1d.Constant(0.0)
    case (Constant(Vector2d(1, 0)), expression) => Expression1d.XComponent2d(expression)
    case (Constant(Vector2d(0, 1)), expression) => Expression1d.YComponent2d(expression)
    case (expression, Constant(Vector2d(1, 0))) => Expression1d.XComponent2d(expression)
    case (expression, Constant(Vector2d(0, 1))) => Expression1d.YComponent2d(expression)
    case (first, second) if (first == second) => first.squaredNorm
    case _ => Expression1d.DotProduct2d(this, that)
  }
}

object Expression2d {
  object Parameter2d extends Expression2d[Point2d]

  case class Constant[T](val value: Vector2d) extends Expression2d[T] {
    override def unary_- : Expression2d[T] = Constant(-value)

    override def squaredNorm: Expression1d[T] = Expression1d.Constant(value.squaredLength)

    override def norm: Expression1d[T] = Expression1d.Constant(value.length)
  }

  case class Negation[T](expression: Expression2d[T]) extends Expression2d[T] {
    override def squaredNorm: Expression1d[T] = expression.squaredNorm

    override def norm: Expression1d[T] = expression.norm
  }

  case class Sum[T](firstExpression: Expression2d[T], secondExpression: Expression2d[T])
    extends Expression2d[T]

  case class Difference[T](firstExpression: Expression2d[T], secondExpression: Expression2d[T])
    extends Expression2d[T] {

    override def unary_- : Expression2d[T] =
      Difference[T](secondExpression, firstExpression)
  }

  case class Product[T](firstExpression: Expression1d[T], secondExpression: Expression2d[T])
    extends Expression2d[T]

  case class Quotient[T](firstExpression: Expression2d[T], secondExpression: Expression1d[T])
    extends Expression2d[T]
}
