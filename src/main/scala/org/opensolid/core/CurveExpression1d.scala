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

abstract class CurveExpression1d {
  import CurveExpression1d._

  def evaluate(t: Double): Double

  def derivative: CurveExpression1d

  def unary_- : CurveExpression1d = Negation(this)

  final def negated: CurveExpression1d = -this

  final def +(that: CurveExpression1d): CurveExpression1d = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue + secondValue)
    case (expression, Zero) => expression
    case (Zero, expression) => expression
    case (first, second) if (first == second) => Constant(2) * first
    case (first, Negation(second)) => first - second
    case (Negation(first), second) => second - first
    case _ => Sum(this, that)
  }

  final def plus(that: CurveExpression1d): CurveExpression1d = this + that

  final def -(that: CurveExpression1d): CurveExpression1d = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue - secondValue)
    case (expression, Zero) => expression
    case (Zero, expression) => -expression
    case (first, second) if (first == second) => Zero
    case (first, Negation(second)) => first + second
    case _ => Difference(this, that)
  }

  final def minus(that: CurveExpression1d): CurveExpression1d = this - that

  final def *(that: CurveExpression1d): CurveExpression1d = (this, that) match {
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue * secondValue)
    case (_, Zero) => Zero
    case (Zero, _) => Zero
    case (expression, One) => expression
    case (One, expression) => expression
    case (expression, NegativeOne) => -expression
    case (NegativeOne, expression) => -expression
    case (first, second) if (first == second) => first.squared
    case (Quotient(a, b), Quotient(c, d)) => (a * c) / (b * d)
    case _ => Product(this, that)
  }

  final def times(that: CurveExpression1d): CurveExpression1d = this * that

  final def /(that: CurveExpression1d): CurveExpression1d = (this, that) match {
    case (_, Zero) => throw new ArithmeticException("Division by zero")
    case (Constant(firstValue), Constant(secondValue)) => Constant(firstValue / secondValue)
    case (Zero, _) => Zero
    case (expression, One) => expression
    case (expression, NegativeOne) => -expression
    case (expression, Constant(value)) => Constant(1.0 / value) * expression
    case (expression, Quotient(numerator, denominator)) => expression * denominator / numerator
    case _ => Quotient(this, that)
  }

  final def dividedBy(that: CurveExpression1d): CurveExpression1d = this / that

  def squared: CurveExpression1d = Square(this)
}

object CurveExpression1d {
  val Zero: CurveExpression1d = Constant(0.0)

  val One: CurveExpression1d = Constant(1.0)

  val NegativeOne: CurveExpression1d = Constant(-1.0)

  val Parameter: CurveExpression1d = Identity

  case class Constant(value: Double) extends CurveExpression1d {
    override def evaluate(t: Double): Double = value

    override def derivative: CurveExpression1d = Zero

    override def unary_- : CurveExpression1d = Constant(-value)

    override def squared: CurveExpression1d = Constant(value * value)
  }

  case object Identity extends CurveExpression1d {
    override def evaluate(t: Double): Double = t

    override def derivative: CurveExpression1d = CurveExpression1d.One
  }

  case class Negation(expression: CurveExpression1d) extends CurveExpression1d {
    override def evaluate(t: Double): Double = -expression.evaluate(t)

    override def derivative: CurveExpression1d = -expression.derivative

    override def unary_- : CurveExpression1d = expression

    override def squared: CurveExpression1d = expression.squared
  }

  case class Sum(firstExpression: CurveExpression1d, secondExpression: CurveExpression1d)
    extends CurveExpression1d {

    override def evaluate(t: Double): Double =
      firstExpression.evaluate(t) + secondExpression.evaluate(t)

    override def derivative: CurveExpression1d =
      firstExpression.derivative + secondExpression.derivative
  }

  case class Difference(firstExpression: CurveExpression1d, secondExpression: CurveExpression1d)
    extends CurveExpression1d {

    override def evaluate(t: Double): Double =
      firstExpression.evaluate(t) - secondExpression.evaluate(t)

    override def derivative: CurveExpression1d =
      firstExpression.derivative - secondExpression.derivative

    override def unary_- : CurveExpression1d = Difference(secondExpression, firstExpression)
  }

  case class Product(firstExpression: CurveExpression1d, secondExpression: CurveExpression1d)
    extends CurveExpression1d {

    override def evaluate(t: Double): Double =
      firstExpression.evaluate(t) * secondExpression.evaluate(t)

    override def derivative: CurveExpression1d =
      firstExpression.derivative * secondExpression + firstExpression * secondExpression.derivative
  }

  case class Quotient(firstExpression: CurveExpression1d, secondExpression: CurveExpression1d)
    extends CurveExpression1d {

    override def evaluate(t: Double): Double =
      firstExpression.evaluate(t) / secondExpression.evaluate(t)

    override def derivative: CurveExpression1d =
      (
        firstExpression.derivative * secondExpression -
        firstExpression * secondExpression.derivative
      ) / secondExpression.squared
  }

  case class Square(expression: CurveExpression1d) extends CurveExpression1d {
    override def evaluate(t: Double): Double = {
      val temp = expression.evaluate(t)
      temp * temp
    }

    override def derivative: CurveExpression1d = Constant(2.0) * expression * expression.derivative
  }
}
