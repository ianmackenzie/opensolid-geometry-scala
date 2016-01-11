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

abstract class ScalarExpression[T, B] {
  import ScalarExpression._

  def evaluate(parameterValue: T): Double

  def bounds(parameterBounds: B): Interval

  def unary_- : ScalarExpression[T, B] = Negation[T, B](this)

  final def negated: ScalarExpression[T, B] = -this

  final def +(that: ScalarExpression[T, B]): ScalarExpression[T, B] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant[T, B](firstValue + secondValue)
    case (expression, Constant(0.0)) => expression
    case (Constant(0.0), expression) => expression
    case (first, second) if (first == second) => Constant[T, B](2) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def plus(that: ScalarExpression[T, B]): ScalarExpression[T, B] = this + that

  final def -(that: ScalarExpression[T, B]): ScalarExpression[T, B] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant[T, B](firstValue - secondValue)
    case (expression, Constant(0.0)) => expression
    case (Constant(0.0), expression) => -expression
    case (first, second) if (first == second) => Constant[T, B](0.0)
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def minus(that: ScalarExpression[T, B]): ScalarExpression[T, B] = this - that

  final def *(that: ScalarExpression[T, B]): ScalarExpression[T, B] = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant[T, B](firstValue * secondValue)
    case (_, Constant(0.0)) => Constant[T, B](0.0)
    case (Constant(0.0), _) => Constant[T, B](0.0)
    case (expression, Constant(1.0)) => expression
    case (Constant(1.0), expression) => expression
    case (expression, Constant(-1.0)) => -expression
    case (Constant(-1.0), expression) => -expression
    case (first, second) if (first == second) => first.squared
    case (Quotient(a, b), Quotient(c, d)) => (a * c) / (b * d)
    case _ => Product(this, that)
  }

  final def times(that: ScalarExpression[T, B]): ScalarExpression[T, B] = this * that

  final def /(that: ScalarExpression[T, B]): ScalarExpression[T, B] = (this, that) match {
    case (_, Constant(0.0)) => throw new ArithmeticException("Division by zero")
    case (Constant(firstValue), Constant(secondValue)) => Constant[T, B](firstValue / secondValue)
    case (Constant(0.0), _) => Constant[T, B](0.0)
    case (expression, Constant(1.0)) => expression
    case (expression, Constant(-1.0)) => -expression
    case (expression, Constant(value)) => Constant[T, B](1.0 / value) * expression
    case (expression, Quotient(numerator, denominator)) => expression * denominator / numerator
    case _ => Quotient(this, that)
  }

  final def dividedBy(that: ScalarExpression[T, B]): ScalarExpression[T, B] = this / that

  def squared: ScalarExpression[T, B] = Square(this)
}

object ScalarExpression {
  val T: ScalarExpression[Double, Interval] = new ScalarExpression[Double, Interval] {
    override def evaluate(parameterValue: Double): Double = parameterValue

    override def bounds(parameterBounds: Interval): Interval = parameterBounds
  }

  val U: ScalarExpression[Point2d, Box2d] = new ScalarExpression[Point2d, Box2d] {
    override def evaluate(parameterValue: Point2d): Double = parameterValue.x

    override def bounds(parameterBounds: Box2d): Interval = parameterBounds.x
  }

  val V: ScalarExpression[Point2d, Box2d] = new ScalarExpression[Point2d, Box2d] {
    override def evaluate(parameterValue: Point2d): Double = parameterValue.y

    override def bounds(parameterBounds: Box2d): Interval = parameterBounds.y
  }

  class Constant[T, B](val value: Double) extends ScalarExpression[T, B] {
    private[this] def interval = Interval(value)

    override def evaluate(parameterValue: T): Double = value

    override def bounds(parameterBounds: B): Interval = interval

    override def unary_- : ScalarExpression[T, B] = Constant[T, B](-value)

    override def squared: ScalarExpression[T, B] = Constant[T, B](value * value)
  }

  object Constant {
    def apply[T, B](value: Double): ScalarExpression[T, B] = new Constant[T, B](value)

    def unapply[T, B](constant: Constant[T, B]): Option[(Double)] = Some(constant.value)
  }

  case class Negation[T, B](expression: ScalarExpression[T, B]) extends ScalarExpression[T, B] {
    override def evaluate(parameterValue: T): Double = -expression.evaluate(parameterValue)

    override def bounds(parameterBounds: B): Interval = -expression.bounds(parameterBounds)

    override def unary_- : ScalarExpression[T, B] = expression

    override def squared: ScalarExpression[T, B] = expression.squared
  }

  case class Sum[T, B](
    firstExpression: ScalarExpression[T, B],
    secondExpression: ScalarExpression[T, B]
  ) extends ScalarExpression[T, B] {

    override def evaluate(parameterValue: T): Double =
      firstExpression.evaluate(parameterValue) + secondExpression.evaluate(parameterValue)

    override def bounds(parameterBounds: B): Interval =
      firstExpression.bounds(parameterBounds) + secondExpression.bounds(parameterBounds)
  }

  case class Difference[T, B](
    firstExpression: ScalarExpression[T, B],
    secondExpression: ScalarExpression[T, B]
  ) extends ScalarExpression[T, B] {

    override def evaluate(parameterValue: T): Double =
      firstExpression.evaluate(parameterValue) - secondExpression.evaluate(parameterValue)

    override def bounds(parameterBounds: B): Interval =
      firstExpression.bounds(parameterBounds) - secondExpression.bounds(parameterBounds)

    override def unary_- : ScalarExpression[T, B] =
      Difference[T, B](secondExpression, firstExpression)
  }

  case class Product[T, B](
    firstExpression: ScalarExpression[T, B],
    secondExpression: ScalarExpression[T, B]
  ) extends ScalarExpression[T, B] {

    override def evaluate(parameterValue: T): Double =
      firstExpression.evaluate(parameterValue) * secondExpression.evaluate(parameterValue)

    override def bounds(parameterBounds: B): Interval =
      firstExpression.bounds(parameterBounds) * secondExpression.bounds(parameterBounds)
  }

  case class Quotient[T, B](
    firstExpression: ScalarExpression[T, B],
    secondExpression: ScalarExpression[T, B]
  ) extends ScalarExpression[T, B] {

    override def evaluate(parameterValue: T): Double =
      firstExpression.evaluate(parameterValue) / secondExpression.evaluate(parameterValue)

    override def bounds(parameterBounds: B): Interval =
      firstExpression.bounds(parameterBounds) / secondExpression.bounds(parameterBounds)
  }

  case class Square[T, B](expression: ScalarExpression[T, B]) extends ScalarExpression[T, B] {
    override def evaluate(parameterValue: T): Double = {
      val temp = expression.evaluate(parameterValue)
      temp * temp
    }

    override def bounds(parameterBounds: B): Interval = expression.bounds(parameterBounds).squared
  }
}
